// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

require('./aggressive_removal_data.es6.js');

foam.CLASS({
  package: 'org.chromium.apis.web',
  name: 'AggressiveRemoval',
  extends: 'org.chromium.apis.web.MetricComputer',
  documentation: `AggressiveRemoval is a class that computes the aggressive
    removal metrics from browserApiDAO. It shows the number of APIs
    that is removed in this browser at least 1 years ago but still exists in
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
      documentation: `Compute aggressive removal value for each browser in
          browsers at the given date.`,
      args: [
        {
          name: 'browsers',
          typeName: 'org.chromium.apis.web.Browser[]',
          documentation: `An array of browsers. These browsers are either
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
                this.mlang.NEQ(this.Browser.BROWSER_NAME, browser.browserName),
                this.mlang.IN(this.Browser.BROWSER_KEY, browserKeys))).select(),
            this.browserDAO.where(this.mlang.AND(
                this.mlang.EQ(this.Browser.BROWSER_NAME, browser.browserName),
                this.mlang.LT(this.Browser.RELEASE_DATE, oneYearAgo),
                this.mlang.EQ(this.Browser.OS_NAME, browser.osName))).select()];
          Promise.all(selectBrowsers).then((result) => {
            let currBrowsers = result[0].a;
            let prevReleaseBrowsers = result[1].a;
            // Return if there is insufficient of historical data.
            if (prevReleaseBrowsers.length < 2) return;
            let browser = prevReleaseBrowsers.pop();
            let removedIfaces = {};
            let promises = [];
            // Find all APIs available this browser before date - 1yr.
            for (let i = 0; i < prevReleaseBrowsers.length; i++) {
              promises.push(prevReleaseBrowsers[i].interfaces.select({
                put: function(iface) {
                  if (!removedIfaces.hasOwnProperty(iface.interfaceKey)) {
                    removedIfaces[iface.interfaceKey] = 0;
                  }
                },
              }));
            }
            // Find removed APIs. (APIs in browsers before date - 1yr
            // set minus APIs in version released just before date - 1yr).
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
                  let numAggressiveRemoval = Object.keys(removedIfaces).filter(
                      (ifaceKey) => {
                    return removedIfaces[ifaceKey] === currBrowsers.length;
                  }).length;
                  this.aggressiveRemovalDAO.put(
                      this.AggressiveRemovalData.create({
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
