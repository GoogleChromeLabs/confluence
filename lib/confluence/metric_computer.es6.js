// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

require('../web_apis/release.es6.js');

foam.CLASS({
  name: 'MetricComputer',
  package: 'org.chromium.apis.web',
  implements: ['foam.mlang.Expressions'],

  documentation: `An abstract class for computing an historical browser metric
      over all release dates for all known browser releases. The run() method
      iterates over all browser release dates and invokes compute() to store
      <number of browsers> metric values. The compute() method stores these
      values given:

      (1) The latest release of each browser as of a particular date;
      (2) The date for which the metric.`,

  requires: [
    'org.chromium.apis.web.Release',
    'foam.dao.ArraySink',
  ],
  imports: [
    'releaseDAO as ctxReleaseDAO',
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
    {
      class: 'foam.dao.DAOProperty',
      name: 'releaseDAO',
      documentation: `DAO of releases used to compute metric. By default, this
          is desktop (i.e., non-mobile) releases in the DAO provided by the
          context.`,
      factory: function() {
        return this.ctxReleaseDAO.where(this.EQ(this.Release.IS_MOBILE, false));
      },
    },
  ],

  methods: [
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
            .select(this.GROUP_BY(
                this.Release.BROWSER_NAME,
                this.ArraySink.create()))
            .then((groups) => {
          return groups.groupKeys.map((bName) => {
            return groups.groups[bName].array[0];
          });
        });
      },
    },
    {
      name: 'run',
      documentation: `Computes the Metric Result for each browser, for each
          release date.`,
      code: function() {
        this.metricLogger.startRun();
        return this.releaseDAO.select(
            this.GROUP_BY(this.Release.BROWSER_NAME, this.GROUP_BY(
                this.Release.OS_NAME,
                this.COUNT())))
            .then((groups) => {
              this.metricLogger.startGatherReleases();
              let numBrowsers = groups.groupKeys.length;
              return this.getOrderedListOfReleaseDates().then((dates) => {
                let promises = [];
                for (let i = 0; i < dates.length; i++) {
                  let date = dates[i];
                  promises.push(this.getLatestReleaseFromEachBrowserAtDate(date)
                      .then((releases) => {
                        // TODO(markdittmer): Perhaps a zero-value should be
                        // stored in the else case here. This would ensure
                        // completeness of data.
                        if (releases.length === numBrowsers)
                          return this.compute(releases, date);
                        else
                          return undefined;
                      }));
                }
                return Promise.all(promises);
              });
            });
      },
    },
    {
      name: 'compute',
      documentation: `Compute <number of known browsers> metric values bound to
          "date", one for each browser.

          E.g.,
          AggressiveRemoval.compute(
              <latest Chrome, Safari, Firefox, Edge as of 2017-01-01>,
              2017-01-01);
          computes an "aggressive removal" value for each of: Chrome, Safari,
          Firefox, and Edge; the values are associated with release releases
          "as of 2017-01-01".`,
      args: [
        {
          name: 'releases',
          typeName: 'org.chromium.apis.web.Release[]',
          documentation: 'The latest release from each browser as of "date".',
        },
        {
          name: 'date',
          typeName: 'Date',
          documentation: `The date for which each browser-specific datum will is
              to be computed.`,
        },
      ],
      code: function(releases, date) {
        throw new Error('MetricComputer.compute() is abstract.');
      },
    },
  ],
});
