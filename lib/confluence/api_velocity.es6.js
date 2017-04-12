// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

require('./api_velocity_data.es6');

foam.CLASS({
  name: 'ApiVelocity',
  package: 'org.chromium.apis.web',
  documentation: `ApiVelocity is a class that computes the API
    Velocity metrics from browserApiDAO. It shows the total
    number of APIs, new APIs and remvoed APIs for each version
    of browser.`,
  requires: [
    'org.chromium.apis.web.Browser',
    'org.chromium.apis.web.WebInterface',
    'org.chromium.apis.web.BrowserWebInterfaceJunction',
    'org.chromium.apis.web.ApiVelocityData',
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
      documentation: `A DAO containing all browser information.`,
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
      name: 'apiVelocityDAO',
      documentation: `This is a DAO that contains ApiVelocityData.`,
      final: true,
      factory: function() {
        return this.EasyDAO.create({
          name: 'apiVelocityDAO',
          of: this.ApiVelocityData,
          daoType: 'MDAO',
        });
      },
    },
    {
      name: 'mlang',
      documentation: 'The mlang expressions singleton.',
      factory: function() {
        return this.ExpressionsSingleton.create();
      },
    },
  ],
  methods: [
    {
      name: 'init',
      documentation: `The init function computes the API Velocity for
          each major browsers contained in the given browserApiDAO and
          store them in apiVelocityDAO.`,
      code: function() {
        this.browserDAO.orderBy(this.Browser.RELEASE_DATE)
            .select(this.mlang.GROUP_BY(
                this.Browser.BROWSER_NAME,
                this.mlang.GROUP_BY(
                    this.Browser.OS_NAME,
                    this.ArraySink.create())))
            .then((groups) => {
              for (let i = 0; i < groups.groupKeys.length; i++) {
                let browserName = groups.groupKeys[i];
                // Use Windows platform if this browser is available in Windows.
                // Use whatever available if Windows is not supported.
                let browserOS = 'Windows';
                if (!groups.groups[browserName].groups.hasOwnProperty(
                    browserOS)) {
                  browserOS = groups.groups[browserName].groupKeys[0];
                }
                let browsers = groups.groups[browserName].groups[browserOS].a;
                let prevIfaces = null;
                for (let i = 0; i < browsers.length; i++) {
                  let browser = browsers[i];
                  // Continue when no previous release is found.
                  if (i === 0) {
                    browser.interfaces.select().then((arraySink) => {
                      prevIfaces = arraySink.a;
                      this.apiVelocityDAO.put(this.ApiVelocityData.create({
                        releaseDate: browser.releaseDate,
                        browserName: browser.browserName,
                        prevBrowser: null,
                        currBrowser: browser,
                        totalApis: arraySink.a.length,
                        newApis: 0,
                        removedApis: 0,
                      }));
                    });
                    continue;
                  }
                  browser.interfaces.select().then((arraySink) => {
                    // prevIfaceDict initialized as all previous, but pared
                    // down when found in curIfaces, yielding only removed
                    // interfaces.
                    let currIfaces = arraySink.a;
                    let prevIfaceDict = {};
                    let newApis = 0;
                    let removedApis = 0;
                    for (let j = 0; j < prevIfaces.length; j++) {
                      prevIfaceDict[prevIfaces[j].interfaceKey] = true;
                    }
                    for (let j = 0; j < currIfaces.length; j++) {
                      let ifaceKey = currIfaces[j].interfaceKey;
                      if (prevIfaceDict[ifaceKey]) {
                        delete prevIfaceDict[ifaceKey];
                      } else {
                        // This interface exists in current version
                        // not in previous version.
                        newApis++;
                      }
                    }
                    removedApis = Object.keys(prevIfaceDict).length;
                    this.apiVelocityDAO.put(this.ApiVelocityData.create({
                      releaseDate: browser.releaseDate,
                      browserName: browser.browserName,
                      prevBrowser: browsers[i - 1],
                      currBrowser: browser,
                      totalApis: currIfaces.length,
                      newApis,
                      removedApis,
                    }));
                  });
                }
              }
            });
      },
    },
  ],
});
