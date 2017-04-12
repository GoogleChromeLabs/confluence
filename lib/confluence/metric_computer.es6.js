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
      name: 'browserApiDAO',
      documentation: `A DAO containing junction objects of Browser and
          WebInterface.`,
      typeName: 'BrowserAPI DAO',
      required: true,
      final: true,
    },
    {
      name: 'browserDAO',
      documentation: `A DAO containing all browser infomation.`,
      typeName: 'Browser DAO',
      required: true,
      final: true,
    },
    {
      name: 'interfaceDAO',
      documentation: `A DAO containing all interface and API pairs.`,
      typeName: 'WebInterface DAO',
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
      name: 'init',
      documentation: `The init function computes the Metric Result
          for each major browsers contained in the given browserApiDAO.`,
      code: function() {
        this.browserDAO.orderBy(this.Browser.RELEASE_DATE).select(
            this.mlang.GROUP_BY(this.Browser.BROWSER_NAME, this.mlang.GROUP_BY(
                this.Browser.OS_NAME,
                this.ArraySink.create()))).then((groups) => {
          let browserList = {};
          for (let i = 0; i < groups.groupKeys.length; i++) {
            let browserName = groups.groupKeys[i];
            let osGroups = groups.groups[browserName];
            // Use Windows platform if this browser is available in Windows.
            // Use whatever available if Windows is not supported.
            let browserOS = 'Windows';
            if (!osGroups.groups.hasOwnProperty(browserOS)) {
              browserOS = osGroups.groupKeys[0];
            }
            browserList[browserName] = osGroups.groups[browserOS].a.map(
                (browser) => {
              return {
                browser,
                date: browser.releaseDate,
              };
            });
          }
          // nextReleaseDate will be the time point that used to
          // caculated the metric result. The first
          // nextReleaseDate will be on the day that
          // all browsers have at least one release version.
          let nextReleaseDate = null;
          Object.keys(browserList).forEach((browser) => {
            if (nextReleaseDate === null ||
                nextReleaseDate < browserList[browser][0].date) {
              nextReleaseDate = browserList[browser][0].date;
            }
          });
          while(nextReleaseDate !== null) {
            // Remove version that is before nextReleaseDate.
            Object.keys(browserList).forEach((browser) => {
              while(browserList[browser][0].date <= nextReleaseDate) {
                if (browserList[browser].length <= 1 ||
                  browserList[browser][1].date > nextReleaseDate) {
                    // Compute metric result for this version
                    // browser at nextReleaseDate time point.
                    browserList[browser][0].date = nextReleaseDate;
                    break;
                }
                browserList[browser].shift();
              }
            });
            this.compute(Object.keys(browserList).map((browser) => {
                return browserList[browser][0].browser;
            }), nextReleaseDate);
            // Update nextReleaseDate to the next most recent release.
            nextReleaseDate = null;
            Object.keys(browserList).reduce((browser, currBrowserName) => {
              if (browserList[currBrowserName].length > 1 &&
                  (nextReleaseDate === null ||
                  browserList[currBrowserName][1].date < nextReleaseDate)) {
                    nextReleaseDate =browserList[currBrowserName][1].date;
                    return browserList[currBrowserName];
              }
              return browser;
            }, browserList[Object.keys(browserList)[0]]).shift();
            // While loop will break if cannot find next release date.
          }
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
        throw new Error('MetricComputer is a abstract class.');
      },
    },
  ],
});
