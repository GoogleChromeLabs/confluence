// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

require('../data_source.es6.js');
require('../fork.es6.js');
require('../web_apis/release.es6.js');
require('./metric_computer_service.es6.js');

foam.CLASS({
  name: 'AbstractMetricComputerRunner',
  package: 'org.chromium.apis.web',
  implements: ['foam.mlang.Expressions'],

  documentation: `Abstract runner that does not rely on any service strategy.`,

  requires: [
    'org.chromium.apis.web.Release',
    'foam.dao.ArraySink',
  ],
  imports: [
    'apiCountDAO?',
    'browserMetricsDAO?',
    'releaseDAO as ctxReleaseDAO',
    'releasePredicate',
  ],

  properties: [
    {
      class: 'foam.dao.DAOProperty',
      name: 'releaseDAO',
      documentation: `DAO of releases used to compute metric. By default, this
          is desktop (i.e., non-mobile) releases in the DAO provided by the
          context.`,
      factory: function() {
        return this.ctxReleaseDAO.where(this.releasePredicate);
      },
    },
  ],

  methods: [
    {
      name: 'run',
      code: function() {
        throw new Error('Call to abstract AbstractMetricComputerRunner.run()');
      },
    },
    {
      name: 'getOrderedListOfReleaseDates',
      documentation: `Get an array of Dates associated with all known browser
          releases. The array is in ascending chronological order.`,
      returns: {
        typeName: 'String[]',
        documentation: `An array of Dates when any major release has a
            new release.`,
      },
      code: function() {
        return this.releaseDAO.orderBy(this.Release.RELEASE_DATE).select(
            this.GROUP_BY(this.Release.RELEASE_DATE,
                this.ArraySink.create())).then((groups) => {
          return groups.groupKeys;
        });
      },
    },
    {
      name: 'getLatestReleaseFromEachBrowserAtDate',
      documentation: `Get the latest release of each browser before the given
          date.`,
      args: [
        {
          name: 'date',
          typeName: 'Date',
          documentation: `The returned latest version of releases are released
              on this date or before`,
        },
      ],
      returns: {
        typeName: 'org.chromium.apis.web.Release[]',
        documentation: `A list of releases which are the latest release before
            the given date.`,
      },
      code: function(date) {
        return this.releaseDAO.orderBy(
            this.DESC(this.Release.RELEASE_DATE))
            .where(this.LTE(this.Release.RELEASE_DATE, date))
            .where(this.NEQ(this.Release.BROWSER_NAME, 'Edge'))
            .select(this.GROUP_BY(
                this.Release.BROWSER_NAME,
                this.ArraySink.create()))
            .then((groups) => {
              return groups.groupKeys.map((browserName) => {
                // Prefer Windows release every time.
                const releases = groups.groups[browserName].array;
                const winReleases = releases
                    .filter((release) => release.osName === 'Windows');
                return winReleases.length > 0 ? winReleases[0] : releases[0];
              });
            });
      },
    },
  ],
});

foam.CLASS({
  name: 'MetricComputerRunner',
  package: 'org.chromium.apis.web',
  extends: 'org.chromium.apis.web.AbstractMetricComputerRunner',
  implements: ['org.chromium.apis.web.ForkBoxFactory'],

  requires: [
    'foam.box.BoxRegistry',
    'foam.box.BroadcastRegistry',
    'foam.box.Context',
    'foam.box.SkeletonBox',
    'foam.core.StubFactorySingleton',
    'foam.dao.ArraySink',
    'org.chromium.apis.web.MetricComputerService',
    'org.chromium.apis.web.MetricComputerType',
    'org.chromium.apis.web.Release',
  ],

  properties: [
    {
      class: 'Enum',
      of: 'org.chromium.apis.web.DataSource',
      name: 'mode',
      required: true,
    },
    {
      class: 'FObjectArray',
      of: 'org.chromium.apis.web.MetricComputerType',
      name: 'metricComputerTypes',
      factory: function() {
        return [
          this.MetricComputerType.LONE_REMOVAL,
          this.MetricComputerType.API_COUNT,
          this.MetricComputerType.BROWSER_SPECIFIC,
          this.MetricComputerType.LONE_OMISSION,
        ];
      },
    },
    {
      name: 'numWorkers',
      value: 16,
    },
    {
      name: 'stubFactory',
      factory: function() {
        return this.StubFactorySingleton.create();
      },
    },
    {
      class: 'FObjectProperty',
      of: 'org.chromium.apis.web.MetricComputerService',
      name: 'metricComputerService',
      factory: function() {
        return this.stubFactory.get(this.MetricComputerService).create({
          delegate: this.metricsContext_.registry.register(
              null, null, this.SkeletonBox.create({
                data: this.MetricComputerService.create({
                // Ensure that service sees same collection of releases as this
                // runner.
                  releasePredicate: this.releasePredicate,
                }),
              })),
        }, this.containerContext_);
      },
    },
    {
      class: 'FObjectProperty',
      of: 'foam.box.Context',
      name: 'containerContext_',
      factory: function() {
        return this.Context.create({
          unsafe: false,
          classWhitelist: require('../../data/class_whitelist.json'),
        });
      },
    },
    {
      class: 'FObjectProperty',
      of: 'foam.box.Context',
      name: 'metricsContext_',
      factory: function() {
        this.validate();

        const ctx = this.Context.create({
          unsafe: false,
          classWhitelist: require('../../data/class_whitelist.json'),
        });
        ctx.registry = this.BroadcastRegistry.create(null, ctx);
        const numWorkers = this.numWorkers;
        const workers = new Array(numWorkers);

        for (let i = 0; i < numWorkers; i++) {
          // Stub BoxRegistry interface over box addressed to new fork.
          workers[i] = this.stubFactory.get(this.BoxRegistry).create({
            delegate: this.getForkBox(this.mode, this.containerContext_),
          }, this.containerContext_);
        }
        ctx.registry.delegates = workers;

        return ctx;
      },
    },
  ],

  methods: [
    {
      name: 'run',
      documentation: `Computes the Metric Result for each browser, for each
          release date.`,
      code: function() {
        return this.releaseDAO.orderBy(this.Release.RELEASE_DATE)
            .select(this.GROUP_BY(this.Release.BROWSER_NAME, this.GROUP_BY(
                this.Release.OS_NAME,
                this.ArraySink.create())))
            .then((groups) => Promise.all([
              this.computeBrowserMetrics(groups),
              this.computeApiCount(groups),
            ]));
      },
    },
    function computeBrowserMetrics(groups) {
      const numBrowsers = groups.groupKeys.length;
      return this.getOrderedListOfReleaseDates().then((dates) => {
        const promises = [];
        for (let i = 0; i < dates.length; i++) {
          const date = dates[i];
          promises.push(
              this.getLatestReleaseFromEachBrowserAtDate(date)
                  .then((releases) => this.computeBrowserMetrics_(numBrowsers,
                      releases,
                      date)));
        }
        return Promise.all(promises);
      });
    },
    function computeApiCount(groups) {
      if (!this.metricComputerTypes.includes(
          this.MetricComputerType.API_COUNT)) {
        return Promise.resolve();
      }

      return Promise.all(groups.groupKeys.map((browserName) => {
        const osGroups = groups.groups[browserName];
        // Use Windows platform if this browser is available in Windows.
        // Use whatever available if Windows is not supported.
        return this.computeApiCount_(
            (osGroups.groups['Windows'] ||
             osGroups.groups[osGroups.groupKeys[0]]).array);
      }));
    },
    function computeBrowserMetrics_(numBrowsers, releases, date) {
      // TODO(markdittmer): Perhaps a zero-value should be
      // stored in the else case here. This would ensure
      // completeness of data.
      if (releases.length !== numBrowsers) {
        return Promise.resolve(this.metricComputerTypes.map((type) => []));
      }

      // ApiCount computed separately.
      const types = this.metricComputerTypes.filter(
          (type) => type !== this.MetricComputerType.API_COUNT);

      // Compute from service, then store results.
      return Promise.all(types.map(
          (type) => this.compute_(type, releases, date)));
    },
    function computeApiCount_(releases) {
      if (releases.length === 0) return undefined;
      const compute = this.compute_
          .bind(this, this.MetricComputerType.API_COUNT);

      const promises = [compute([releases[0]], releases[0].releaseDate)];
      for (let i = 0; i < releases.length - 1; i++) {
        const prev = releases[i];
        const next = releases[i + 1];
        promises.push(compute([prev, next], next.releaseDate));
      }
      return Promise.all(promises);
    },
    function compute_(type, releases, date) {
      return this.metricComputerService.compute(type, releases, date)
          .then((metrics) => Promise.all(
              (metrics || []).map((value) => this[type.daoName].put(value))));
    },
    function getForkNodeParams_() {
      return ['--max_old_space_size=4096'];
    },
    function getForkScriptPath_() {
      return require('path').resolve(`${__dirname}/../../main/forkScript.js`);
    },
  ],
});
