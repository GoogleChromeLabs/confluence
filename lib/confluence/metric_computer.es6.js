// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

foam.CLASS({
  name: 'MetricComputer',
  package: 'org.chromium.apis.web',
  documentation: `MetricComputer is a class that process all browsers
      with different vendors and call compute methods on each date
      when there is a new release of any vendor's browser.`,
  requires: [
    'org.chromium.apis.web.Browser',
    'org.chromium.apis.web.WebInterface',
    'org.chromium.apis.web.BrowserWebInterfaceJunction',
    'foam.mlang.ExpressionsSingleton',
    'foam.dao.ArraySink',
    'foam.dao.EasyDAO',
  ],
  properties: [
    {
      class: 'FObjectProperty',
      of: 'foam.dao.DAO',
      name: 'browserApiDAO',
      documentation: `A DAO containing junction objects of Browser and
          WebInterface.`,
      required: true,
      final: true,
    },
    {
      class: 'FObjectProperty',
      of: 'foam.dao.DAO',
      name: 'browserDAO',
      documentation: `A DAO containing each browsers' version and
          environment metadata.`,
      required: true,
      final: true,
    },
    {
      class: 'FObjectProperty',
      of: 'foam.dao.DAO',
      name: 'interfaceDAO',
      documentation: `A DAO containing all interface and API pairs.`,
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
      name: 'getOrderedListOfBrowserReleaseDates',
      documentation: `Get an array of Dates for which there is a release
          for any of major browsers.`,
      returns: {
        typeName: 'String[]',
        documentation: `An array of Dates when any major browser has a
            new release.`,
      },
      code: function() {
        return this.browserDAO.orderBy(this.Browser.RELEASE_DATE).select(
            this.mlang.GROUP_BY(this.Browser.RELEASE_DATE,
            this.ArraySink.create())).then((groups) => {
          return groups.groupKeys;
        });
      },
    },
    {
      name: 'getLatestBrowserFromEachVendorAtDate',
      documentation: `Get the latest version of browser before the given date
          for each major browser vendors.`,
      args: [
        {
          name: 'date',
          typeName: 'Date',
          documentation: `The returned latest version of browsers are released
              on this date or before`,
        },
      ],
      returns: {
        typeName: 'org.chromium.apis.web.Browser[]',
        documentation: `A list of browsers which are the latest release before
            the given date.`,
      },
      code: function(date) {
        return this.browserDAO.orderBy(
            this.mlang.DESC(this.Browser.RELEASE_DATE))
            .where(this.mlang.LTE(this.Browser.RELEASE_DATE, date))
            .select(this.mlang.GROUP_BY(
                this.Browser.BROWSER_NAME,
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
      documentation: `Computes the Metric Result for each major browsers
          contained in the given browserApiDAO.`,
      code: function() {
        return this.browserDAO.select(
            this.mlang.GROUP_BY(this.Browser.BROWSER_NAME, this.mlang.GROUP_BY(
                this.Browser.OS_NAME,
                this.mlang.COUNT())))
            .then((groups) => {
              let numVendors = groups.groupKeys.length;
              return this.getOrderedListOfBrowserReleaseDates().then((dates) => {
                let promises = [];
                for (let i = 0; i < dates.length; i++) {
                  let date = dates[i];
                  promises.push(this.getLatestBrowserFromEachVendorAtDate(date)
                      .then((browsers) => {
                        if (browsers.length === numVendors)
                          return this.compute(browsers, date);
                      }));
                }
                return Promise.all(promises);
              });
            });
      },
    },
    {
      name: 'compute',
      args: [
        {
          name: 'browsers',
          typeName: 'org.chromium.apis.web.Browser[]',
          documentation: `An array of brwsers. These browsers are either
              released at given date, or it is the most recent release
              before given date.`,
        },
        {
          name: 'date',
          typeName: 'Date',
          documentation: `The vendor specific value will be calculated at
              given date.`,
        },
      ],
      code: function(browsers, date) {
        throw new Error('MetricComputer.compute() is abstract.');
      },
    },
  ],
});
