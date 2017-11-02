// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

require('../web_apis/release.es6.js');
require('./api_velocity_data.es6');
require('./set_ops.es6.js');

foam.CLASS({
  name: 'ApiVelocity',
  package: 'org.chromium.apis.web',
  implements: ['org.chromium.mlang.Expressions'],

  documentation: `Metric computer for API counts that Nth release to N-1th
      release of a browser.

      Counts are: {<total APIs>, <removed APIs>, <added APIs>}.`,

  requires: [
    'foam.dao.ArraySink',
    'foam.dao.EasyDAO',
    'org.chromium.apis.web.ApiVelocityData',
    'org.chromium.apis.web.Release',
  ],
  imports: [
    'apiVelocityDAO',
    'info',
    'releaseDAO',
  ],

  methods: [
    {
      name: 'compute',
      documentation: `Compute metrics for a group of releases from a particular
          browser/platform.`,
      code: function(releases, date) {
        const numReleases = releases.length;
        foam.assert(numReleases === 1 || numReleases === 2,
                    'ApiVelocity.compute(): Expected 1 or 2 releases');
        const lastReleaseDate = releases[releases.length - 1].releaseDate;
        foam.assert(
            date.getTime() === lastReleaseDate.getTime(),
            `ApiVelocity.compute(): Expected date to match last release date:
                Date: ${date}
                Last release date: ${lastReleaseDate}`);

        if (numReleases === 1)
          return this.computeForSingleRelease_(releases[0]);
        else
          return this.computeForPair_(releases[0], releases[1]);
      },
    },
    {
      name: 'computeForSingleRelease_',
      documentation: `Compute single metric value for first release in a
          sequence.`,
      code: function(release) {
        this.info(`Computing ApiVelocity for single release ${release.id}`);
        return release.interfaces.dao.select(this.COUNT())
            .then(count => {
              this.info(`Computed ApiVelocity for single release ${release.id}`);
              return this.apiVelocityDAO.put(this.ApiVelocityData.create({
                releaseDate: release.releaseDate,
                browserName: release.browserName,
                currRelease: release,
                totalApis: count.value,
              }));
            });
      },
    },
    {
      name: 'computeForPair_',
      documentation: `Compute single metric value for "next" release, based on
          "prev" as previous browser release.`,
      code: function(prev, next) {
        this.info(`Computing ApiVelocity for release pair ${prev.id}, ${next.id}`);
        return Promise.all([
          // [0]: API count.
          next.interfaces.dao.select(this.COUNT()),
          // [1]: Removed count.
          prev.interfaces.dao.select(
              this.SET_MINUS(next.interfaces.dao, this.COUNT())),
          // [2]: Added count.
          next.interfaces.dao.select(
              this.SET_MINUS(prev.interfaces.dao, this.COUNT())),
        ]).then(results => {
          this.info(`Computed ApiVelocity for release pair ${prev.id}, ${next.id}`);
          return this.apiVelocityDAO.put(this.ApiVelocityData.create({
            releaseDate: next.releaseDate,
            browserName: next.browserName,
            currRelease: next,
            prevRelease: prev,
            totalApis: results[0].value,
            removedApis: results[1].value,
            newApis: results[2].value,
          }));
        });
      },
    },
  ],
});
