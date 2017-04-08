// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

require('./vendor_specific_data.es6.js');

foam.CLASS({
  name: 'VendorSpecific',
  package: 'org.chromium.apis.web',
  documentation: `FailureToShip is a class that computes the Vendor Specific
    API metrics from browserApiDAO. It shows the number of APIs
    that only available in this browser.`,
  requires: [
    'org.chromium.apis.web.Browser',
    'org.chromium.apis.web.WebInterface',
    'org.chromium.apis.web.BrowserWebInterfaceJunction',
    'org.chromium.apis.web.VendorSpecificData',
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
      name: 'vendorSpecificDAO',
      documentation: `This is a DAO that contains vendor specific data.`,
      final: true,
      factory: function() {
        return this.EasyDAO.create({
          name: 'vendorSpecificDAO',
          of: this.VendorSpecificData,
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
      documentation: `The init function computes the Failure To Ship Metric
        for each major browsers contained in the given browserApiDAO and
        store them in failureToShipDAO.`,
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
            // caculated the failure-to-ship result. The first
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
                      // Compute failure to ship value for this version browser
                      // at nextReleaseDate time point.
                      browserList[browser][0].date = nextReleaseDate;
                      break;
                  }
                  browserList[browser].shift();
                }
              });
              this.computeVendorSpecific(Object.keys(browserList)
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
      name: 'computeVendorSpecific',
      code: function(browsers, date) {
        for (let i = 0; i < browsers.length; i++) {
          let browser = browsers[i];
          let year = date.getFullYear();
          let month = date.getMonth();
          let day = date.getDate();
          let previousYear = new Date(year - 1, month, day);
          let browserKeys = browsers.map((browser) => browser.browserKey);
          // Find all browsers that
          //   - a previous version of the browser.
          //   - some other browser but released in previous one year range.
          //   - if no release in one year, uese most recent browser.
          this.browserDAO.where(this.mlang.OR(
            this.mlang.AND(
              this.mlang.IN(this.Browser.BROWSER_KEY, browserKeys)),
            this.mlang.AND(
              this.mlang.EQ(this.Browser.BROWSER_NAME, browser.browserName),
              this.mlang.GT(this.Browser.RELEASE_DATE, previousYear),
              this.mlang.LT(this.Browser.RELEASE_DATE, date)),
            this.mlang.AND(
              this.mlang.GT(this.Browser.RELEASE_DATE, previousYear),
              this.mlang.LT(this.Browser.RELEASE_DATE, date))))
          .select().then((result) => {
            let browserIface = {};
            let numPrevReleases = 1;
            let allIfaces = {};
            let promises = [];
            let prevReleaseBrowsers = [];
            let comparedBrowsers = [];
            for (let i = 0; i < result.a.length; i++) {
              if (result.a[i].browserName !== browser.browserName) {
                comparedBrowsers.push(result.a[i]);
              } else if (result.a[i].browserKey !== browser.browserKey) {
                numPrevReleases++;
                prevReleaseBrowsers.push(result.a[i]);
              }
              promises.push(result.a[i].interfaces.select({
                put: function(iface) {
                  if (result.a[i].browserName === browser.browserName) {
                    if (!browserIface.hasOwnProperty(iface.interfaceKey)) {
                      browserIface[iface.interfaceKey] = 0;
                    }
                    browserIface[iface.interfaceKey]++;
                  } else {
                    if (!allIfaces.hasOwnProperty(iface.interfaceKey)) {
                      allIfaces[iface.interfaceKey] = true;
                    }
                  }
                },
              }));
            }
            Promise.all(promises).then(() => {
              // Find interfaces that all other vendor's browsers supports
              // but not available in any version of this verndor's browsers.
              let numVendorSpecific = 0;
              for (let ifaceKey in browserIface) {
                if (browserIface[ifaceKey] !== numPrevReleases) continue;
                if (!allIfaces.hasOwnProperty(ifaceKey)) {
                  numVendorSpecific++;
                }
              }
              this.vendorSpecificDAO.put(this.VendorSpecificData.create(
                {
                  browserName: browser.browserName,
                  browserVersion: browser.browserVersion,
                  browser,
                  prevReleaseBrowsers,
                  comparedBrowsers,
                  numVendorSpecific,
                  date,
                }));
            });
          });
        }
      },
    },
  ],
});
