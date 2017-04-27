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
    'foam.dao.AnonymousSink',
    'foam.dao.ArraySink',
    'foam.dao.EasyDAO',
    'foam.mlang.ExpressionsSingleton',
    'org.chromium.apis.web.AggressiveRemovalData',
    'org.chromium.apis.web.Browser',
    'org.chromium.apis.web.BrowserWebInterfaceJunction',
    'org.chromium.apis.web.WebInterface',
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
      code: function(browsers, date) {
        let computeForBrowsers = [];
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
          computeForBrowsers.push(Promise.all(selectBrowsers).then((result) => {
            let currBrowsers = result[0].a;
            let prevReleaseBrowsers = result[1].a;
            // Return if there is insufficient of historical data. This method
            // returns Promise<Sink> (result of select() call).
            if (prevReleaseBrowsers.length < 2)
              return Promise.resolve(this.ArraySink.create());

            // TODO(markdittmer): This assumes that browsers are in
            // releaseDate-order. That assumption doesn't appear to be
            // documented or enforced.
            let latestPrevReleaseBrowser = prevReleaseBrowsers.pop();
            let removedIfaces = {};
            let promises = [];
            // Find all APIs available this browser before date -1yr.
            for (let i = 0; i < prevReleaseBrowsers.length; i++) {
              promises.push(prevReleaseBrowsers[i].interfaces.select(
                  this.AnonymousSink.create({sink: {put: function(_, iface) {
                      removedIfaces[iface.interfaceKey] = 0;
                  }}})));
            }

            // Helper: Perform SET_MINUS(removedIFaces, browserDAO's ifaces).
            function removeFromRemoved(browserDAO) {
              return browserDAO.interfaces.select(this.AnonymousSink.create({
                sink: {put: function(_, iface) {
                  if (removedIfaces.hasOwnProperty(iface.interfaceKey)) {
                    delete removedIfaces[iface.interfaceKey];
                  }
                }},
              }));
            }
            // Find removed APIs: APIs in browsers before date -1yr set-minus
            // APIs in version released just before date -1yr. Also set-minus
            // latest version of the same browser to avoid penalizing vendors
            // that reintroduce an API after premature removal.
            return Promise.all(promises).then(() => Promise.all([
              removeFromRemoved.call(this, latestPrevReleaseBrowser),
              removeFromRemoved.call(this, browser),
            ])).then(() => {
              // Find the interfaces that still exists in today's
              // other browsers.
              promises = [];
              for (let i = 0; i < currBrowsers.length; i++) {
                promises.push(currBrowsers[i].interfaces.select(
                    this.AnonymousSink.create({
                      sink: {put: function(_, iface) {
                        if (removedIfaces.hasOwnProperty(
                            iface.interfaceKey)) {
                          removedIfaces[iface.interfaceKey]++;
                        }
                      }},
                    })));
              }
              return Promise.all(promises).then(() => {
                // Find number of APIs still exists in all today's
                // other version of browsers.
                let numAggressiveRemoval = Object.keys(removedIfaces).filter(
                    (ifaceKey) => {
                      return removedIfaces[ifaceKey] === currBrowsers.length;
                    }).length;
                return this.aggressiveRemovalDAO.put(
                    this.AggressiveRemovalData.create({
                      browserName: browser.browserName,
                      browserOneYearAgo: latestPrevReleaseBrowser,
                      prevReleaseBrowsers,
                      currBrowsers,
                      numAggressiveRemoval,
                      date,
                    }));
              });
            });
          }));
        }
        return Promise.all(computeForBrowsers);
      },
    },
  ],
});
