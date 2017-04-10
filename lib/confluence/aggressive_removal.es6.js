// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

require('./aggressive_removal_data.es6.js');

foam.CLASS({
  name: 'AggressiveRemoval',
  package: 'org.chromium.apis.web',
  documentation: `AggressiveRemoval is a class that computes the aggressive
    removal metrics from browserApiDAO. It shows the number of APIs
    that is removed in this browser at least 2 years ago but still exits in
    other browsers.`,
  requires: [
    'org.chromium.apis.web.Browser',
    'org.chromium.apis.web.WebInterface',
    'org.chromium.apis.web.BrowserWebInterfaceJunction',
    'org.chromium.apis.web.AggressiveRemovalData',
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
    {
      name: 'mlang',
      documentation: `Object contains mlang expressions, it supports
        logic operations such as AND and OR and math operations such
        as IN, EQ.`,
      factory: function() {
        return this.ExpressionsSingleton.create();
      },
    },
  ],
  methods: [
    {
      name: 'init',
      documentation: `The init function computes the Aggressive Removal Metric
        for each major browsers contained in the given browserApiDAO and
        store them in aggressiveRemovalDAO.`,
      code: function() {
        this.browserDAO.orderBy(this.Browser.RELEASE_DATE)
          .select(this.mlang.GROUP_BY(
            this.Browser.BROWSER_NAME,
            this.mlang.GROUP_BY(
              this.Browser.OS_NAME,
              this.ArraySink.create())))
          .then((groups) => {
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
              browserList[browserName] = osGroups.groups[browserOS].a
                .map((browser) => {
                  return {
                    browser,
                    date: browser.releaseDate,
                  };
                });
            }
            // nextReleaseDate will be the time point that used to
            // caculated the aggressive removal result. The first
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
                      // Compute aggressive removal value for this version
                      // browser at nextReleaseDate time point.
                      browserList[browser][0].date = nextReleaseDate;
                      break;
                  }
                  browserList[browser].shift();
                }
              });
              this.computeAggressiveRemoval(Object.keys(browserList)
                .map((browser) => {
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
      name: 'computeAggressiveRemoval',
      documentation: `Compute aggressive removal value for each browser in
           browsers at the given date.`,
      args: [
        {
          name: 'browsers',
          typeName: 'Array of Browser',
          documentation: `An array of brwsers. These browser are either
              released at given date, or it is the most recent release
              before given date.`,
        },
        {
          name: 'date',
          typeName: 'Date',
          documentation: `The aggressive removal value will be calculated at
              given date.`,
        },
      ],
      code: function(browsers, date) {
        for (let i = 0; i < browsers.length; i++) {
          let browser = browsers[i];
          let year = date.getFullYear();
          let month = date.getMonth();
          let day = date.getDate();
          let oneYearAgo = new Date(year - 1, month, day);
          let browserKeys = browsers.map((browser) => browser.browserKey);
          let selectBrowsers = [
            this.browserDAO.where(this.mlang.AND(
              this.mlang.IN(this.Browser.BROWSER_KEY, browserKeys),
              this.mlang.NEQ(this.Browser.BROWSER_NAME, browser.browserName)))
            .select(),
            this.browserDAO.where(this.mlang.AND(
              this.mlang.EQ(this.Browser.BROWSER_NAME, browser.browserName),
              this.mlang.EQ(this.Browser.OS_NAME, browser.osName),
              this.mlang.LT(this.Browser.RELEASE_DATE, oneYearAgo)))
            .select()];
          Promise.all(selectBrowsers).then((result) => {
            let currBrowsers = result[0].a;
            let prevReleaseBrowsers = result[1].a;
            // Return if there is insufficient of historical data.
            if (prevReleaseBrowsers.length < 2) return;
            let browser = prevReleaseBrowsers.pop();
            let removedIfaces = {};
            let promises = [];
            // Find all APIs available this browser between date - 2years
            // to date - 3years period.
            for (let i = 0; i < prevReleaseBrowsers.length; i++) {
              promises.push(prevReleaseBrowsers[i].interfaces.select({
                put: function(iface) {
                  if (!removedIfaces.hasOwnProperty(iface.interfaceKey)) {
                    removedIfaces[iface.interfaceKey] = 0;
                  }
                },
              }));
            }
            // Find removed APIs. (APIs in browsers between date - 2years
            // to date - 3years set minus APIs in version released just
            // before date -2years).
            Promise.all(promises).then(() => {
              browser.interfaces.select({
                put: function(iface) {
                  if (removedIfaces.hasOwnProperty(iface.interfaceKey)) {
                    delete removedIfaces[iface.interfaceKey];
                  }
                },
              }).then(() => {
                // Find the interfaces that still exists in today's
                // other browsers.
                promises = [];
                for (let i = 0; i < currBrowsers.length; i++) {
                  promises.push(currBrowsers[i].interfaces.select({
                    put: function(iface) {
                      if (removedIfaces.hasOwnProperty(iface.interfaceKey)) {
                        removedIfaces[iface.interfaceKey]++;
                      }
                    },
                  }));
                }
                Promise.all(promises).then(() => {
                  // Find number of APIs still exists in all today's
                  // other version of browsers.
                  let numAggressiveRemoval = Object.keys(removedIfaces)
                    .filter((ifaceKey) => {
                      return removedIfaces[ifaceKey] === currBrowsers.length;
                    }).length;
                  this.aggressiveRemovalDAO.put(
                    this.AggressiveRemovalData.create(
                      {
                        browserName: browser.browserName,
                        browserOneYearAgo: browser,
                        prevReleaseBrowsers,
                        currBrowsers,
                        numAggressiveRemoval,
                        date,
                      }));
                });
              });
            });
          });
        }
      },
    },
  ],
});
