// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

require('../web_apis/release.es6.js');
require('./browser_metric_data.es6.js');
require('./metric_computer.es6.js');

foam.CLASS({
  package: 'org.chromium.apis.web',
  name: 'LoneRemoval',
  extends: 'org.chromium.apis.web.MetricComputer',
  implements: ['org.chromium.mlang.Expressions'],

  documentation: `Count of APIs removed from a browser that are still
      part of every other browser's API surface. Count is computed with
      a one-year grace period for other browsers to also remove the API
      before it counts against the browser that was first to remove.`,

  requires: [
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
      name: 'loneRemovalDAO',
      documentation: `This is a DAO that contains LONE_REMOVAL-type
          BrowserMetricData.`,
      final: true,
      factory: function() {
        return this.browserMetricsDAO.where(this.EQ(
            this.BrowserMetricData.TYPE,
            this.BrowserMetricDataType.LONE_REMOVAL));
      },
    },
  ],

  methods: [
    {
      name: 'compute',
      documentation: `Compute lone removal value for each release in
          "releases", given "date".`,
      code: function(releases, date) {
        return Promise.all(releases.map(
            release => this.computeForRelease(releases, date, release)));
      },
    },
    {
      name: 'computeForRelease',
      documentation: 'Compute lone removal value for a single release.',
      code: function(releases, date, release) {
        this.info(`Computing lone removal: ${date}, ${release.id}`);
        let otherReleaseIDs = releases.map(rls => rls.releaseKey)
            .filter(id => !foam.util.equals(id, release.id));
        return Promise.all([
          // Current releases from browser other than "release".
          this.releaseDAO.where(this.IN(this.Release.ID, otherReleaseIDs))
              .orderBy(this.DESC(this.Release.RELEASE_DATE))
              .select(),
          // Releases of the "release" browser from before the one-year grace
          // period.
          this.releaseDAO.where(this.AND(
                  this.EQ(this.Release.BROWSER_NAME, release.browserName),
                  this.LT(this.Release.RELEASE_DATE,
                                new Date(date.getFullYear() - 1,
                                         date.getMonth(),
                                         date.getDate())),
                  this.EQ(this.Release.OS_NAME, release.osName)))
                      .orderBy(this.DESC(this.Release.RELEASE_DATE))
                      .select(),
        ]).then(currentOtherAndPrevOwn => {
          let currentOtherReleases = currentOtherAndPrevOwn[0].array;
          let prevOwnReleases = currentOtherAndPrevOwn[1].array;
          return this.computeFromReleases(
              date, release, currentOtherReleases, prevOwnReleases);
        });
      },
    },
    {
      name: 'computeFromReleases',
      documentation: `Compute lone removal value for "release", given
          arrays of "current" releases of other browsers and "previous"
          releases (from "release" browser) that are older than the one-year
          grace period. The removal must have occurred before the grace period,
          so the data point is actually associated with the latest release in
          the "previous" collection. Lone removals are as follows:

          1. Shipped in one of "previous" releases;
          2. Not shipped in latest release in "previous";
          3. Still not shipped in "release";
          4. Shipped in all "current" releases (of other browsers).`,
      code: function(date, release, current, previous) {
        return this.compatDAO.select().then(s => {
          return this.testComputeFromReleases(s.array, date, release, current, previous);
        })
      },
    },
    {
      name:'testComputeFromReleases',
      code: function(compatData, date, release, current, previous) {
        compatData;

        // If there are no other current releases, or fewer than two previous
        // releases from "release" browser, then the metric must be 0.
        if (current.length === 1 || previous.length < 2)
          return this.recordZero_(date, release, current, previous);

        let prevLessOne = Array.from(previous);
        let postRemoval = prevLessOne.shift();

        const props = this.CompatData.getAxiomsByClass(this.CompatProperty);
        const currentProps = props.filter(p => current
            .some(r => foam.util.equals(p.release, r)));
        const prevProps = props.filter(p => prevLessOne
            .some(r => foam.util.equals(p.release, r)));
        const postRemovalProp = props
            .find(p => foam.util.equals(p.release, postRemoval));
        const releaseProp = props
            .find(p => foam.util.equals(p.release, release));
        
        const E = this.ExpressionsSingleton.create();
        let query = E.AND(
          // Shipped in previous releases of this browser.
          E.OR.apply(E, prevProps.map(p => E.EQ(p, true))),
          // Not shipped in last release of previous releases of this browser.
          E.EQ(postRemovalProp, false),
          // Still not shipped in current release of this browser.
          E.EQ(releaseProp, false),
          // Shipped in latest release of all other browsers.
          E.AND.apply(E, currentProps.map(p => E.EQ(p, true))));

      return this.compatDAO.where(query).select(this.COUNT())
          .then(countSink => this.loneRemovalDAO.put(
            this.BrowserMetricData.create({
              type: this.BrowserMetricDataType.LONE_REMOVAL,
              browserName: release.browserName,
              release: postRemoval,
              prevReleases: prevLessOne,
              comparedReleases: current,
              value: countSink.value,
              date,
            })));
      },
    },
    {
      name: 'recordZero_',
      documentation: `Store 0-count in "loneRemovalDAO" for given release
          data.`,
      code: function(date, release, current, previous) {
        // TODO(markdittmer): Currently the storage + UI layers expect
        // missing data when insufficient releases exist. Should we instead
        // store a 0-count?
      },
    },
  ],
});
