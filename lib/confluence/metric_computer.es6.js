// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

foam.CLASS({
  name: 'MetricComputer',
  package: 'org.chromium.apis.web',
  documentation: `An abstract class for computing an historical browser metric
      over all release dates for all known browser releases. The run() method
      iterates over all browser release dates and invokes compute() to store
      <number of browsers> metric values. The compute() method stores these
      values given:

      (1) The latest release of each browser as of a particular date;
      (2) The date for which the metric.`,
  requires: [
    'org.chromium.apis.web.Release',
    'org.chromium.apis.web.WebInterface',
    'org.chromium.apis.web.ReleaseWebInterfaceJunction',
    'foam.mlang.ExpressionsSingleton',
    'foam.dao.ArraySink',
    'foam.dao.EasyDAO',
  ],
  properties: [
    {
      class: 'FObjectProperty',
      of: 'foam.dao.DAO',
      name: 'releaseApiDAO',
      documentation: `A DAO containing junction objects of Release and
          WebInterface.`,
      required: true,
      final: true,
    },
    {
      class: 'FObjectProperty',
      of: 'foam.dao.DAO',
      name: 'releaseDAO',
      documentation: `A DAO containing all known browser releases.`,
      required: true,
      final: true,
    },
    {
      class: 'FObjectProperty',
      of: 'foam.dao.DAO',
      name: 'interfaceDAO',
      documentation: `A DAO containing all known interface and API pairs.`,
      required: true,
      final: true,
    },
    {
      name: 'mlang',
      documentation: `The mlang singleton expression.`,
      factory: function() {
        return this.ExpressionsSingleton.create();
      },
    },
  ],
  methods: [
    {
      name: 'getOrderedListOfReleaseReleaseDates',
      documentation: `Get an array of Dates associated with all known browser
          releases. The array is in ascending chronological order.`,
      returns: {
        typeName: 'String[]',
        documentation: `An array of Dates when any major release has a
            new release.`,
      },
      code: function() {
        return this.releaseDAO.orderBy(this.Release.RELEASE_DATE).select(
            this.mlang.GROUP_BY(this.Release.RELEASE_DATE,
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
            this.mlang.DESC(this.Release.RELEASE_DATE))
            .where(this.mlang.LTE(this.Release.RELEASE_DATE, date))
            .select(this.mlang.GROUP_BY(
                this.Release.BROWSER_NAME,
                this.ArraySink.create()))
            .then((groups) => {
          return groups.groupKeys.map((bName) => {
            return groups.groups[bName].a[0];
          });
        });
      },
    },
    {
      name: 'run',
      documentation: `Computes the Metric Result for each browser, for each
          release date.`,
      code: function() {
        return this.releaseDAO.select(
            this.mlang.GROUP_BY(this.Release.BROWSER_NAME, this.mlang.GROUP_BY(
                this.Release.OS_NAME,
                this.mlang.COUNT())))
            .then((groups) => {
              let numBrowsers = groups.groupKeys.length;
              return this.getOrderedListOfReleaseReleaseDates().then((dates) => {
                let promises = [];
                for (let i = 0; i < dates.length; i++) {
                  let date = dates[i];
                  promises.push(this.getLatestReleaseFromEachBrowserAtDate(date)
                      .then((releases) => {
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
