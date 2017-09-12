// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

require('../web_apis/release.es6.js');
require('./api_velocity_data.es6');
require('./metric_logger.es6.js');
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
    'org.chromium.apis.web.MetricLogger',
    'org.chromium.apis.web.Release',
  ],
  imports: [
    'apiVelocityDAO',
    'info',
    'releaseDAO',
  ],

  properties: [
    {
      class: 'FObjectProperty',
      of: 'org.chromium.apis.web.MetricLogger',
      documentation: 'Custom logger for tracing metric computations.',
      name: 'metricLogger',
      factory: function() {
        return this.MetricLogger.create({
          name: this.cls_.id,
        });
      },
    },
  ],

  methods: [
    {
      name: 'run',
      documentation: `Computes the API Velocity for each browser, for each
          release date.`,
      code: function() {
        return this.groupReleases().then(this.computeFromGroups);
      },
    },
    {
      name: 'groupReleases',
      documentation: `Produce a nested GROUP_BY sink that groups releases by
          browser name, then by OS name. Releases are ordered by release date.`,
      code: function() {
        this.info('API velocity grouping releases');
        return this.releaseDAO.orderBy(this.Release.RELEASE_DATE).select(
          this.GROUP_BY(this.Release.BROWSER_NAME, this.GROUP_BY(
              this.Release.OS_NAME, this.ArraySink.create())));
      },
    },
    {
      name: 'compute',
      documentation: `Compute metrics for a group of releases from a particular
          browser/platform.`,
      code: function(releases) {
        if (releases.length === 0) return undefined;
        this.metricLogger.startCompute(releases[0].releaseDate, releases[0]);
        var promises = [this.computeForSingleRelease_(releases[0])];
        for (var i = 0; i < releases.length - 1; i++) {
          promises.push(this.computeForPair_(releases[i], releases[i + 1]));
        }
        return Promise.all(promises);
      },
    },
    {
      name: 'computeForSingleRelease_',
      documentation: `Compute single metric value for first release in a
          sequence.`,
      code: function(release) {
        this.metricLogger.startComputeForReleases(
            release.releaseDate, release, [], []);
        return release.interfaces.dao.select(this.COUNT())
            .then(count => this.apiVelocityDAO.put(this.ApiVelocityData.create({
              releaseDate: release.releaseDate,
              browserName: release.browserName,
              currRelease: release,
              totalApis: count.value,
            })));
      },
    },
    {
      name: 'computeForPair_',
      documentation: `Compute single metric value for "next" release, based on
          "prev" as previous browser release.`,
      code: function(prev, next) {
        this.metricLogger.startComputeForReleases(
            next.releaseDate, next, [], [prev]);
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
          this.metricLogger.finishComputeForReleases(
              next.releaseDate, next, [], [prev],
              results.map(result => result.value));
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

  listeners: [
    {
      name: 'computeFromGroups',
      documentation: `Compute metrics based on groups produced by
          "groupReleases". Implemented as listener to pre-bind to "this".`,
      code: function(groups) {
        return Promise.all(groups.groupKeys.map(browserName => {
          let osGroups = groups.groups[browserName];
          // Use Windows platform if this browser is available in Windows.
          // Use whatever available if Windows is not supported.
          return this.compute(
            (osGroups.groups['Windows'] ||
             osGroups.groups[osGroups.groupKeys[0]]).array);
        }));
      },
    },
  ],
});
