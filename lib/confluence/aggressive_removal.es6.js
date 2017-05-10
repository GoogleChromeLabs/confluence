// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

require('./aggressive_removal_data.es6.js');
require('./set_ops.es6.js');

foam.CLASS({
  package: 'org.chromium.apis.web',
  name: 'AggressiveRemoval',
  extends: 'org.chromium.apis.web.MetricComputer',
  documentation: `Count of APIs removed from a browser that are still
      part of every other browser's API surface. Count is computed with
      a one-year grace period for other browsers to also remove the API
      before it counts against the browser that was first to remove.`,
  requires: [
    'foam.dao.EasyDAO',
    'org.chromium.apis.web.AggressiveRemovalData',
    'org.chromium.apis.web.Release',
  ],
  properties: [
    {
      name: 'aggressiveRemovalDAO',
      documentation: `This is a DAO that contains AggressiveRemovalData.`,
      final: true,
      factory: function() {
        return this.EasyDAO.create({
          name: 'aggressiveRemovalDAO',
          of: this.AggressiveRemovalData,
          daoType: 'MDAO',
        });
      },
    },
  ],
  methods: [
    {
      name: 'compute',
      documentation: `Compute aggressive removal value for each release in
          "releases", given "date".`,
      code: function(releases, date) {
        return Promise.all(releases.map(
            release => this.computeForRelease(releases, date, release)));
      },
    },
    {
      name: 'computeForRelease',
      documentation: 'Compute aggressive removal value for a single release.',
      code: function(releases, date, release) {
        let otherReleaseIDs = releases.map(rls => rls.releaseKey)
            .filter(id => !foam.util.equals(id, release.id));
        return Promise.all([
          // Current releases from browser other than "release".
          this.releaseDAO.where(this.mlang.IN(this.Release.ID, otherReleaseIDs))
              .orderBy(this.mlang.DESC(this.Release.RELEASE_DATE))
              .select(),
          // Releases of the "release" browser from before the one-year grace
          // period.
          this.releaseDAO.where(this.mlang.AND(
                  this.mlang.EQ(this.Release.BROWSER_NAME, release.browserName),
                  this.mlang.LT(this.Release.RELEASE_DATE,
                                new Date(date.getFullYear() - 1,
                                         date.getMonth(),
                                         date.getDate())),
                  this.mlang.EQ(this.Release.OS_NAME, release.osName)))
                      .orderBy(this.mlang.DESC(this.Release.RELEASE_DATE))
                      .select(),
        ]).then(currentOtherAndPrevOwn => {
          let currentOtherReleases = currentOtherAndPrevOwn[0].a;
          let prevOwnReleases = currentOtherAndPrevOwn[1].a;
          return this.computeFromReleases(
              date, release, currentOtherReleases, prevOwnReleases);
        });
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
        return firstCurrent.interfaces.select(
            // (1) APIs in each "current" browser and old releases of
            //     "release" browser:
            //     INTERSECT(<interfaces for each item in "current">,
            //         UNION(<interfaces of "release" relases before grace
            //             period>))
            this.mlang.INTERSECT(
                currentLessOne.map(brwsr => brwsr.interfaces).concat([
                  this.mlang.UNION.apply(
                      this.mlang,
                      prevLessOne.map(brwsr => brwsr.interfaces))
                ]),
                // (2) {(1)} \ <APIs in "release" browser after "old"> \ <APIs
                //         in current release of "release" browser>
                this.mlang.SET_MINUS(
                    [postRemoval.interfaces, release.interfaces],
                    // Count APIs in set.
                    this.mlang.COUNT())))
            // Store count of APIs in computation (2).
            .then(count => {
              return this.aggressiveRemovalDAO.put(
                  this.AggressiveRemovalData.create({
                    browserName: release.browserName,
                    releaseOneYearAgo: postRemoval,
                    prevReleases: prevLessOne,
                    currReleases: current,
                    numAggressiveRemoval: count.value,
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
