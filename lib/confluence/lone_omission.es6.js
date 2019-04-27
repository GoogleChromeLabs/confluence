// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

require('../web_apis/release.es6.js');
require('./browser_metric_data.es6.js');
require('./metric_computer.es6.js');

foam.CLASS({
  package: 'org.chromium.apis.web',
  name: 'LoneOmission',
  extends: 'org.chromium.apis.web.MetricComputer',
  implements: ['foam.mlang.Expressions'],

  documentation: `Metric computer for count of APIs that have been shipping
      in all other browsers for a one-year grace period, but have never been
      shipped in this browser.`,

  requires: [
    'foam.dao.AnonymousSink',
    'foam.dao.EasyDAO',
    'foam.mlang.ExpressionsSingleton',
    'org.chromium.apis.web.BrowserMetricData',
    'org.chromium.apis.web.BrowserMetricDataType',
    'org.chromium.apis.web.generated.CompatData',
    'org.chromium.apis.web.CompatProperty',
    'org.chromium.apis.web.Release',
  ],
  imports: [
    'browserMetricsDAO',
    'compatDAO',
    'info',
  ],

  properties: [
    {
      class: 'foam.dao.DAOProperty',
      name: 'loneOmissionDAO',
      documentation: `This is a DAO that contains loneOmissionData.`,
      final: true,
      factory: function() {
        return this.browserMetricsDAO.where(this.EQ(
            this.BrowserMetricData.TYPE,
            this.BrowserMetricDataType.LONE_OMISSION));
      },
    },
  ],
  methods: [
    {
      name: 'compute',
      documentation: `Compute lone omission value for each release in
          "releases", given "date".`,
      code: function(releases, date) {
        return Promise.all(releases.map(
            (release) => this.computeForRelease(releases, date, release)));
      },
    },
    {
      name: 'computeForRelease',
      documentation: 'Compute lone omission value for a single release.',
      code: function(releases, date, release) {
        this.info(`Computing lone omission: ${date}, ${release.id}`);
        const otherReleases = releases
            .filter((rls) => !foam.util.equals(rls.id, release.id));
        const gracePeriodStart = new Date(date.getFullYear() - 1,
            date.getMonth(),
            date.getDate());
        // Compared releases: Releases of browsers other than "release"'s
        // browser, that were released during the grace period.
        const comparedReleases = this.releaseDAO.where(
            this.AND(
                // LTE: Include other releases on the same date as "release".
                this.LTE(this.Release.RELEASE_DATE, date),
                this.GTE(this.Release.RELEASE_DATE, gracePeriodStart),
                this.OR.apply(
                    this,
                    otherReleases.map((rls) => this.AND(
                        this.EQ(this.Release.BROWSER_NAME, rls.browserName),
                        this.EQ(this.Release.OS_NAME, rls.osName))))));
        // Previous releases: Releases of "release"'s browser on or prior to
        // "date".
        const prevReleases = this.releaseDAO.where(
            this.AND(
                // LT: Exclude "release".
                this.LT(this.Release.RELEASE_DATE, date),
                this.EQ(this.Release.BROWSER_NAME, release.browserName),
                this.EQ(this.Release.OS_NAME, release.osName)));
        return Promise.all([comparedReleases.select(), prevReleases.select()])
            .then((results) => this.computeFromGracePeriodReleases(
                results[0].array, results[1].array, date, release));
      },
    },
    {
      name: 'computeFromGracePeriodReleases',
      documentation: `Compute lone omission value based on browsers released
          during grace period.`,
      code: function(comparedReleases, prevReleases, date, release) {
        const otherBrowserProps = comparedReleases
            .map((r) => this.getPropertyForRelease(r));
        const thisBrowserReleases = prevReleases.concat([release]);
        const thisBrowserProps = thisBrowserReleases
            .map((r) => this.getPropertyForRelease(r));

        // Compute:
        // "APIs never shipped in browser-of-interest, but shipped by
        //     all other browsers throughout grace period".
        const query = this.AND.apply(this, otherBrowserProps
            .map((p) => this.EQ(p, true)).concat(thisBrowserProps
                .map((p) => this.EQ(p, false))));
        return this.compatDAO.where(query).select(this.COUNT())
            .then((countSink) => {
              return this.loneOmissionDAO.put(
                  this.BrowserMetricData.create({
                    type: this.BrowserMetricDataType.LONE_OMISSION,
                    browserName: release.browserName,
                    value: countSink.value,
                    release,
                    date,
                    prevReleases,
                    comparedReleases,
                  }));
            });
      },
    },
  ],
});
