// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

require('../web_apis/release.es6.js');
require('../web_apis/release_interface_relationship.es6.js');
require('../web_apis/web_interface.es6.js');
require('./browser_metric_data.es6.js');
require('./metric_computer.es6.js');
require('./set_ops.es6.js');

foam.CLASS({
  package: 'org.chromium.apis.web',
  name: 'AggressiveRemoval',
  extends: 'org.chromium.apis.web.MetricComputer',
  implements: ['org.chromium.mlang.Expressions'],

  documentation: `Count of APIs removed from a browser that are still
      part of every other browser's API surface. Count is computed with
      a one-year grace period for other browsers to also remove the API
      before it counts against the browser that was first to remove.`,

  requires: [
    'foam.dao.EasyDAO',
    'org.chromium.apis.web.BrowserMetricData',
    'org.chromium.apis.web.BrowserMetricDataType',
    'org.chromium.apis.web.Release',
  ],
  imports: [
    'info',
    'releaseDAO',
    'browserMetricsDAO',
  ],

  properties: [
    {
      name: 'aggressiveRemovalDAO',
      documentation: `This is a DAO that contains AGGRESSIVE_REMOVAL-type
          BrowserMetricData.`,
      final: true,
      factory: function() {
        return this.browserMetricsDAO.where(this.EQ(
            this.BrowserMetricData.TYPE,
            this.BrowserMetricDataType.AGGRESSIVE_REMOVAL));
      },
    },
  ],

  methods: [
    {
      name: 'compute',
      documentation: `Compute aggressive removal value for each release in
          "releases", given "date".`,
      code: function(releases, date) {
        this.info('Aggressive removal: Starting compute');
        return Promise.all(releases.map(
            release => this.computeForRelease(releases, date, release)));
      },
    },
    {
      name: 'computeForRelease',
      documentation: 'Compute aggressive removal value for a single release.',
      code: function(releases, date, release) {
        this.info('Aggressive removal: Starting compute for release ' +
            release.id);
        let otherReleaseIDs = releases.map(rls => rls.releaseKey)
            .filter(id => !foam.util.equals(id, release.id));

        // TODO(markdittmer): Intended query is:
        // where(IN(ID,otherReleaseIDs)).orderBy(DESC(RELEASE_DATE))
        // but IN() is a disjunction, which Datastore does not currently
        // support.
        return Promise.all(otherReleaseIDs.map(id => this.releaseDAO.find(id)))
            // Current releases from browser other than "release", sorted by
            // descending release date.
            .then(releases => releases.sort(
                this.Release.RELEASE_DATE.compare).reverse())
            .then(currentOtherReleases =>
              // Releases of the "release" browser from before the one-year
              // grace period, sorted by descending release date.
              this.releaseDAO.where(this.AND(
                      this.EQ(this.Release.BROWSER_NAME, release.browserName),
                      this.LT(this.Release.RELEASE_DATE,
                                    new Date(date.getFullYear() - 1,
                                             date.getMonth(),
                                             date.getDate())),
                      this.EQ(this.Release.OS_NAME, release.osName)))
                          .orderBy(this.DESC(this.Release.RELEASE_DATE))
                          .select()
              .then(prevOwnReleasesSink => {
                foam.assert(
                    currentOtherReleases.every(release => !!release),
                    'releaseDAO.find(<known-other-release>) should be truthy');
                let prevOwnReleases = prevOwnReleasesSink.array;
                return this.computeFromReleases(
                    date, release, currentOtherReleases, prevOwnReleases);
              }));
      },
    },
    {
      name: 'computeFromReleases',
      documentation: `Compute aggressive removal value for "release", given
          arrays of "current" releases of other browsers and "previous"
          releases (from "release" browser) that are older than the one-year
          grace period.

          Let releaseAPIs = <interfaces in "release">
          Let curAPIs = INTERSECT(<interfaces for each item in "current">)
          Let cutOffAPIs = <interfaces from newest release in "previous">
          Let oldAPIs = UNION(<interfaces from "previous" releases other than
                               newest (release associated with "cutOffAPIs")>)

          Then:
          Removed up to grace period (and not added back) = removedAPIs =
              oldAPIs \ cutOffAPIs \ releaseAPIs
          Aggressively removed APIs =
              INTERSECT(curAPIs, removedAPIs)`,
      code: function(date, release, current, previous) {
        this.info('Aggressive removal: Starting compute for releases ' +
            release.id +
            ' current: ' + current.length +
            ' previous: ' + previous.length);
        // If there are no other current releases, or fewer than two previous
        // releases from "release" browser, then the metric must be 0.
        if (current.length === 1 || previous.length < 2)
          return this.recordZero_(date, release, current, previous);

        let prevLessOne = Array.from(previous);
        let postRemoval = prevLessOne.shift();
        let currentLessOne = Array.from(current);
        let firstCurrent = currentLessOne.shift();

        // INTERSECT() decorates sink from a select(), so to compute
        // intersection-of-<foos>, use first-foo.select(INTERSECT(rest-foos)).
        return firstCurrent.interfaces.dao.select(
            // (1) APIs in each "current" browser and old releases of
            //     "release" browser:
            //     INTERSECT(<interfaces for each item in "current">,
            //         UNION(<interfaces of "release" relases before grace
            //             period>))
            this.INTERSECT(
                currentLessOne.map(brwsr => brwsr.interfaces.dao).concat([
                  this.UNION.apply(
                      this,
                      prevLessOne.map(brwsr => brwsr.interfaces.dao))
                ]),
                // (2) {(1)} \ <APIs in "release" browser after "old"> \ <APIs
                //         in current release of "release" browser>
                this.SET_MINUS(
                    [postRemoval.interfaces.dao, release.interfaces.dao],
                    // Count APIs in set.
                    this.COUNT())))
            // Store count of APIs in computation (2).
            .then(count => {
              this.info('Aggressive removal: Completed compute for releases ' +
                  release.id +
                  ' current: ' + current.length +
                  ' previous: ' + previous.length +
                  ' value: ' + count.value);
              return this.aggressiveRemovalDAO.put(
                  this.BrowserMetricData.create({
                    type: this.BrowserMetricDataType.AGGRESSIVE_REMOVAL,
                    browserName: release.browserName,
                    release: postRemoval,
                    prevReleases: prevLessOne,
                    comparedReleases: current,
                    value: count.value,
                    date,
                  }));
            });
      },
    },
    {
      name: 'recordZero_',
      documentation: `Store 0-count in "aggressiveRemovalDAO" for given release
          data.`,
      code: function(date, release, current, previous) {
        // TODO(markdittmer): Currently the storage + UI layers expect
        // missing data when insufficient releases exist. Should we instead
        // store a 0-count?
      },
    },
  ],
});
