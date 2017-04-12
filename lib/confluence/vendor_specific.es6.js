// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

require('./vendor_specific_data.es6.js');

foam.CLASS({
  package: 'org.chromium.apis.web',
  name: 'VendorSpecific',
  extends: 'org.chromium.apis.web.MetricComputer',
  documentation: `VendorSpecific is a class that computes the vendor-specific
    API metrics from browserApiDAO. It shows the number of APIs from this
    browser vendor. These APIs were shipped by a vendor at least 1 year ago,
    and haven't since been removed (i.e., appear in all of the last year's
    releases), and have not been visible in any other browser vendor during
    the last year's releases.`,
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
  ],
  methods: [
    {
      name: 'compute',
      documentation: `Compute vendor specific API value for each browser in
          browsers at the given date.`,
      code: function(browsers, date) {
        for (let i = 0; i < browsers.length; i++) {
          let browser = browsers[i];
          let year = date.getFullYear();
          let month = date.getMonth();
          let day = date.getDate();
          let previousYear = new Date(year - 1, month, day);
          let browserKeys = browsers.map((browser) => browser.browserKey);
          // Find all browsers that satisfy one of:
          //   - a previous version of the browser.
          //   - some other browser but released in previous one year range.
          //   - if no release in one year, use most recent browser.
          this.browserDAO.where(this.mlang.OR(
              this.mlang.AND(
                  this.mlang.IN(this.Browser.BROWSER_KEY, browserKeys)),
              this.mlang.AND(
                  this.mlang.EQ(this.Browser.BROWSER_NAME, browser.browserName),
                  this.mlang.EQ(this.Browser.OS_NAME, browser.osName),
                  this.mlang.GT(this.Browser.RELEASE_DATE, previousYear),
                  this.mlang.LT(this.Browser.RELEASE_DATE, date)),
              this.mlang.AND(
                  this.mlang.GT(this.Browser.RELEASE_DATE, previousYear),
                  this.mlang.LT(this.Browser.RELEASE_DATE, date))
              )).select().then((result) => {
            // browserIface are a map of the form:
            //     {interfaceKey: <number of appearance in preview releases.>}
            // Interface with number of appearance equals to numPrevReleases
            // are considered as interface never been removed.
            let browserIface = {};
            let numPrevReleases = 1;
            // allOtherIfaces are a map of interfaceKey that appears
            // in other vendors' browser.
            let allOtherIfaces = {};
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
                    allOtherIfaces[iface.interfaceKey] = true;
                  }
                },
              }));
            }
            Promise.all(promises).then(() => {
              // Find interfaces that all this browser have shipped and not
              // removed for at least a year but not available in any other
              // vendors' browser.
              let numVendorSpecific = 0;
              for (let ifaceKey in browserIface) {
                if (!browserIface.hasOwnProperty(ifaceKey)) continue;
                // Browser vendor shipping the API must have consistently
                // shipped it in all "numPrevReleases" releases
                if (browserIface[ifaceKey] !== numPrevReleases) continue;
                if (!allOtherIfaces.hasOwnProperty(ifaceKey)) {
                  numVendorSpecific++;
                }
              }
              this.vendorSpecificDAO.put(this.VendorSpecificData.create({
                browserName: browser.browserName,
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
