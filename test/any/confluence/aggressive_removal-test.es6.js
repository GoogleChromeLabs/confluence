// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

describe('AggressiveRemoval', function() {
  let testCtx;
  beforeEach(function() {
    foam.CLASS({
      name: 'Controller',
      package: 'org.chromium.apis.web.test',

      requires: [
        'foam.dao.EasyDAO',
        'org.chromium.apis.web.Browser',
        'org.chromium.apis.web.WebInterface',
        'org.chromium.apis.web.BrowserWebInterfaceJunction',
      ],
      exports: [
        'browserDAO',
        'webInterfaceDAO',
        'browserWebInterfaceJunctionDAO',
      ],

      properties: [
        {
          class: 'foam.dao.DAOProperty',
          name: 'browserDAO',
          factory: function() {
            return this.EasyDAO.create({
              name: 'browserDAO',
              of: this.Browser,
              daoType: 'ARRAY',
            });
          },
        },
        {
          class: 'foam.dao.DAOProperty',
          name: 'webInterfaceDAO',
          factory: function() {
            return this.EasyDAO.create({
              name: 'webInterfaceDAO',
              of: this.WebInterface,
              daoType: 'ARRAY',
            });
          },
        },
        {
          class: 'foam.dao.DAOProperty',
          name: 'browserWebInterfaceJunctionDAO',
          factory: function() {
            return this.EasyDAO.create({
              name: 'browserWebInterfaceJunctionDAO',
              of: this.BrowserWebInterfaceJunction,
              daoType: 'ARRAY',
            });
          },
        },
      ],
    });
    testCtx = foam.lookup('org.chromium.apis.web.test.Controller')
        .create().__subContext__;
  });

  it('should handle simple case', function(done) {
    const Browser = testCtx.lookup('org.chromium.apis.web.Browser');
    const WebInterface = testCtx.lookup('org.chromium.apis.web.WebInterface');
    const AggressiveRemoval =
        testCtx.lookup('org.chromium.apis.web.AggressiveRemoval');
    const browsers = testCtx.browserDAO;
    let alpha1, alpha2, alpha3, beta, charlie, aggressiveRemoval;
    // Instantiate browser releases:
    // Alpha 1, 2, 3: ~1 year apart.
    // Beta, Charlie: same year as Alpha 3.
    Promise.all([
      browsers.put(Browser.create({
        browserName: 'Alpha',
        browserVersion: '1',
        osName: 'Windows',
        osVersion: '10',
        releaseDate: '2015-01-01T00:00:00.000Z'
      }, testCtx)),
      browsers.put(Browser.create({
        browserName: 'Alpha',
        browserVersion: '2',
        osName: 'Windows',
        osVersion: '10',
        releaseDate: '2016-01-02T00:00:00.000Z'
      }, testCtx)),
      browsers.put(Browser.create({
        browserName: 'Alpha',
        browserVersion: '3',
        osName: 'Windows',
        osVersion: '10',
        releaseDate: '2017-01-03T00:00:00.000Z'
      }, testCtx)),
      browsers.put(Browser.create({
        browserName: 'Beta',
        browserVersion: '1',
        osName: 'Windows',
        osVersion: '10',
        releaseDate: '2017-01-04T00:00:00.000Z'
      }, testCtx)),
      browsers.put(Browser.create({
        browserName: 'Charlie',
        browserVersion: '1',
        osName: 'Windows',
        osVersion: '10',
        releaseDate: '2017-01-05T00:00:00.000Z'
      }, testCtx)),
    ]).then(function(browsersArray) {
      alpha1 = browsersArray[0];
      alpha2 = browsersArray[1];
      alpha3 = browsersArray[2];
      beta = browsersArray[3];
      charlie = browsersArray[4];

      // One API, shipped by Alpha, Beta, and Charlie, removed in Alpha 2,
      // more than a year after Alpha 3, Beta, and Charlie releases.
      // That is, API ships in Alpha 1, Beta, and Charlie.
      const ifaces = testCtx.webInterfaceDAO;
      const Junction =
          foam.lookup('org.chromium.apis.web.BrowserWebInterfaceJunction');
      const junctions = testCtx.browserWebInterfaceJunctionDAO;
      const iface = WebInterface.create({
        interfaceName: 'Alpha',
        apiName: 'removesIn2',
      }, testCtx);
      return Promise.all([
        ifaces.put(iface),
        junctions.put(Junction.create({
          id: [alpha1.id, iface.id],
          sourceId: alpha1.id,
          targetId: iface.id,
        })),
        junctions.put(Junction.create({
          id: [beta.id, iface.id],
          sourceId: beta.id,
          targetId: iface.id,
        })),
        junctions.put(Junction.create({
          id: [charlie.id, iface.id],
          sourceId: charlie.id,
          targetId: iface.id,
        })),
      ]);
    }).then(function() {
      // Setup and run aggressive removal metric calculation.
      aggressiveRemoval = AggressiveRemoval.create({
        browserDAO: testCtx.browserDAO,
        interfaceDAO: testCtx.webInterfaceDAO,
        browserApiDAO: testCtx.browserWebInterfaceJunctionDAO,
      }, testCtx);
      return aggressiveRemoval.run();
    }).then(function() {
      return aggressiveRemoval.aggressiveRemovalDAO.select();
    }).then(function(sink) {
      var array = sink.a;
      // One data point satisfies computation constraints:
      // 1. Computed from date with at least one version from each vendor,
      // 2. Computed from date with at least two versions >1yr-old from vendor
      //    whose removals are under analysis.
      expect(array.length).toBe(1);
      var ar = array[0];
      expect(ar.browserName).toBe('Alpha');
      expect(ar.numAggressiveRemoval).toBe(1);
      // First date that all vendors have a release: Charlie release date.
      expect(ar.date).toEqual(charlie.releaseDate);
      expect(foam.util.equals(ar.browserOneYearAgo, alpha2)).toBe(true);
      expect(foam.util.equals(ar.prevReleaseBrowsers, [alpha1])).toBe(true);
      expect(foam.util.equals(ar.currBrowsers.sort(), [beta, charlie].sort()))
          .toBe(true);
      done();
    });
  });

  it('should not capture removal less than a year old', function(done) {
    const Browser = testCtx.lookup('org.chromium.apis.web.Browser');
    const WebInterface = testCtx.lookup('org.chromium.apis.web.WebInterface');
    const AggressiveRemoval =
        testCtx.lookup('org.chromium.apis.web.AggressiveRemoval');
    const browsers = testCtx.browserDAO;
    let alpha0, alpha1, alpha2, alpha3, beta, charlie, aggressiveRemoval;
    // Instantiate browser releases:
    // Alpha 0, 1, 2, 3: ~1 year apart.
    // Beta, Charlie: same year as Alpha 3.
    // Here, Alpha 2 released less than a year before Alpha 3, Beta, or Charlie.
    // Alpha 0 is introduced to hit case where there are two Alpha versions old
    // enough to compute Alpha's aggressive removals.
    Promise.all([
      browsers.put(Browser.create({
        browserName: 'Alpha',
        browserVersion: '0',
        osName: 'Windows',
        osVersion: '10',
        releaseDate: '2014-01-01T00:00:00.000Z'
      }, testCtx)),
      browsers.put(Browser.create({
        browserName: 'Alpha',
        browserVersion: '1',
        osName: 'Windows',
        osVersion: '10',
        releaseDate: '2015-01-01T00:00:00.000Z'
      }, testCtx)),
      browsers.put(Browser.create({
        browserName: 'Alpha',
        browserVersion: '2',
        osName: 'Windows',
        osVersion: '10',
        releaseDate: '2016-01-06T00:00:00.000Z'
      }, testCtx)),
      browsers.put(Browser.create({
        browserName: 'Alpha',
        browserVersion: '3',
        osName: 'Windows',
        osVersion: '10',
        releaseDate: '2017-01-03T00:00:00.000Z'
      }, testCtx)),
      browsers.put(Browser.create({
        browserName: 'Beta',
        browserVersion: '1',
        osName: 'Windows',
        osVersion: '10',
        releaseDate: '2017-01-04T00:00:00.000Z'
      }, testCtx)),
      browsers.put(Browser.create({
        browserName: 'Charlie',
        browserVersion: '1',
        osName: 'Windows',
        osVersion: '10',
        releaseDate: '2017-01-05T00:00:00.000Z'
      }, testCtx)),
    ]).then(function(browsersArray) {
      alpha0 = browsersArray[0];
      alpha1 = browsersArray[1];
      alpha2 = browsersArray[2];
      alpha3 = browsersArray[3];
      beta = browsersArray[4];
      charlie = browsersArray[5];

      // One API, shipped by Alpha, Beta, and Charlie, removed in Alpha 2,
      // but less than a year after Alpha 3, Beta, and Charlie releases.
      // That is, API ships in Alpha 0, Alpha 1, Beta, and Charlie.
      const ifaces = testCtx.webInterfaceDAO;
      const Junction =
          foam.lookup('org.chromium.apis.web.BrowserWebInterfaceJunction');
      const junctions = testCtx.browserWebInterfaceJunctionDAO;
      const iface = WebInterface.create({
        interfaceName: 'Alpha',
        apiName: 'removesIn2',
      }, testCtx);
      return Promise.all([
        ifaces.put(iface),
        junctions.put(Junction.create({
          id: [alpha0.id, iface.id],
          sourceId: alpha0.id,
          targetId: iface.id,
        })),
        junctions.put(Junction.create({
          id: [alpha1.id, iface.id],
          sourceId: alpha1.id,
          targetId: iface.id,
        })),
        junctions.put(Junction.create({
          id: [beta.id, iface.id],
          sourceId: beta.id,
          targetId: iface.id,
        })),
        junctions.put(Junction.create({
          id: [charlie.id, iface.id],
          sourceId: charlie.id,
          targetId: iface.id,
        })),
      ]);
    }).then(function() {
      // Setup and run aggressive removal metric calculation.
      aggressiveRemoval = AggressiveRemoval.create({
        browserDAO: testCtx.browserDAO,
        interfaceDAO: testCtx.webInterfaceDAO,
        browserApiDAO: testCtx.browserWebInterfaceJunctionDAO,
      }, testCtx);
      return aggressiveRemoval.run();
    }).then(function() {
      return aggressiveRemoval.aggressiveRemovalDAO.select();
    }).then(function(sink) {
      var array = sink.a;
      // One data point satisfies computation constraints:
      // 1. Computed from date with at least one version from each vendor,
      // 2. Computed from date with at least two versions >1yr-old from vendor
      //    whose removals are under analysis.
      expect(array.length).toBe(1);
      var ar = array[0];
      expect(ar.browserName).toBe('Alpha');
      // Removal in Alpha 2 not counted: it's less than 1yr-old.
      expect(ar.numAggressiveRemoval).toBe(0);
      // First date that all vendors have a release: Charlie release date.
      expect(ar.date).toEqual(charlie.releaseDate);
      expect(foam.util.equals(ar.browserOneYearAgo, alpha1)).toBe(true);
      expect(foam.util.equals(ar.prevReleaseBrowsers, [alpha0])).toBe(true);
      expect(foam.util.equals(ar.currBrowsers.sort(), [beta, charlie].sort()))
          .toBe(true);
      done();
    });
  });

  it('should capture old removals', function(done) {
    const Browser = testCtx.lookup('org.chromium.apis.web.Browser');
    const WebInterface = testCtx.lookup('org.chromium.apis.web.WebInterface');
    const AggressiveRemoval =
        testCtx.lookup('org.chromium.apis.web.AggressiveRemoval');
    const browsers = testCtx.browserDAO;
    let alpha0, alpha1, alpha2, alpha3, beta, charlie, aggressiveRemoval;
    // Instantiate browser releases:
    // Alpha 0, 1, 2, 3: ~1 year apart.
    // Beta, Charlie: same year as Alpha 3.
    // Alpha 0 is introduced to get two removals from different years.
    Promise.all([
      browsers.put(Browser.create({
        browserName: 'Alpha',
        browserVersion: '0',
        osName: 'Windows',
        osVersion: '10',
        releaseDate: '2014-01-01T00:00:00.000Z'
      }, testCtx)),
      browsers.put(Browser.create({
        browserName: 'Alpha',
        browserVersion: '1',
        osName: 'Windows',
        osVersion: '10',
        releaseDate: '2015-01-02T00:00:00.000Z'
      }, testCtx)),
      browsers.put(Browser.create({
        browserName: 'Alpha',
        browserVersion: '2',
        osName: 'Windows',
        osVersion: '10',
        releaseDate: '2016-01-03T00:00:00.000Z'
      }, testCtx)),
      browsers.put(Browser.create({
        browserName: 'Alpha',
        browserVersion: '3',
        osName: 'Windows',
        osVersion: '10',
        releaseDate: '2017-01-04T00:00:00.000Z'
      }, testCtx)),
      browsers.put(Browser.create({
        browserName: 'Beta',
        browserVersion: '1',
        osName: 'Windows',
        osVersion: '10',
        releaseDate: '2017-01-05T00:00:00.000Z'
      }, testCtx)),
      browsers.put(Browser.create({
        browserName: 'Charlie',
        browserVersion: '1',
        osName: 'Windows',
        osVersion: '10',
        releaseDate: '2017-01-06T00:00:00.000Z'
      }, testCtx)),
    ]).then(function(browsersArray) {
      alpha0 = browsersArray[0];
      alpha1 = browsersArray[1];
      alpha2 = browsersArray[2];
      alpha3 = browsersArray[3];
      beta = browsersArray[4];
      charlie = browsersArray[5];

      // Two APIs, shipped by Alpha, Beta, and Charlie, one removed in Alpha
      // 1, another removed in Alpha 2, both removed over a year before Alpha
      // 3, Beta, and Charlie releases.
      const ifaces = testCtx.webInterfaceDAO;
      const Junction =
          foam.lookup('org.chromium.apis.web.BrowserWebInterfaceJunction');
      const junctions = testCtx.browserWebInterfaceJunctionDAO;
      const iface1 = WebInterface.create({
        interfaceName: 'Alpha',
        apiName: 'removesIn1',
      }, testCtx);
      const iface2 = WebInterface.create({
        interfaceName: 'Alpha',
        apiName: 'removesIn2',
      }, testCtx);
      return Promise.all([
        ifaces.put(iface1),
        ifaces.put(iface2),

        // iface1 removed in Alpha 1.
        junctions.put(Junction.create({
          id: [alpha0.id, iface1.id],
          sourceId: alpha0.id,
          targetId: iface1.id,
        })),
        junctions.put(Junction.create({
          id: [beta.id, iface1.id],
          sourceId: beta.id,
          targetId: iface1.id,
        })),
        junctions.put(Junction.create({
          id: [charlie.id, iface1.id],
          sourceId: charlie.id,
          targetId: iface1.id,
        })),

        // iface2 removed in Alpha 2.
        junctions.put(Junction.create({
          id: [alpha0.id, iface2.id],
          sourceId: alpha0.id,
          targetId: iface2.id,
        })),
        junctions.put(Junction.create({
          id: [alpha1.id, iface2.id],
          sourceId: alpha1.id,
          targetId: iface2.id,
        })),
        junctions.put(Junction.create({
          id: [beta.id, iface2.id],
          sourceId: beta.id,
          targetId: iface2.id,
        })),
        junctions.put(Junction.create({
          id: [charlie.id, iface2.id],
          sourceId: charlie.id,
          targetId: iface2.id,
        })),
      ]);
    }).then(function() {
      // Setup and run aggressive removal metric calculation.
      aggressiveRemoval = AggressiveRemoval.create({
        browserDAO: testCtx.browserDAO,
        interfaceDAO: testCtx.webInterfaceDAO,
        browserApiDAO: testCtx.browserWebInterfaceJunctionDAO,
      }, testCtx);
      return aggressiveRemoval.run();
    }).then(function() {
      return aggressiveRemoval.aggressiveRemovalDAO.select();
    }).then(function(sink) {
      var array = sink.a;
      // One data point satisfies computation constraints:
      // 1. Computed from date with at least one version from each vendor,
      // 2. Computed from date with at least two versions >1yr-old from vendor
      //    whose removals are under analysis.
      expect(array.length).toBe(1);
      var ar = array[0];
      expect(ar.browserName).toBe('Alpha');
      // Both removals counted.
      expect(ar.numAggressiveRemoval).toBe(2);
      // First date that all vendors have a release: Charlie release date.
      expect(ar.date).toEqual(charlie.releaseDate);
      expect(foam.util.equals(ar.browserOneYearAgo, alpha2)).toBe(true);
      expect(foam.util.equals(ar.prevReleaseBrowsers.sort(),
                              [alpha0, alpha1].sort()))
                                  .toBe(true);
      expect(foam.util.equals(ar.currBrowsers.sort(), [beta, charlie].sort()))
          .toBe(true);
      done();
    });
  });

  it('should capture accumulate old removals', function(done) {
    const Browser = testCtx.lookup('org.chromium.apis.web.Browser');
    const WebInterface = testCtx.lookup('org.chromium.apis.web.WebInterface');
    const AggressiveRemoval =
        testCtx.lookup('org.chromium.apis.web.AggressiveRemoval');
    const browsers = testCtx.browserDAO;
    let alpha0, alpha1, alpha2, alpha3, beta, charlie, aggressiveRemoval;
    // Instantiate browser releases:
    // Alpha 0, 1, 2, 3: ~1 year apart.
    // Beta, Charlie: same year as Alpha 2.
    // Alpha 0 is introduced to get two removals from different years.
    Promise.all([
      browsers.put(Browser.create({
        browserName: 'Alpha',
        browserVersion: '0',
        osName: 'Windows',
        osVersion: '10',
        releaseDate: '2014-01-01T00:00:00.000Z'
      }, testCtx)),
      browsers.put(Browser.create({
        browserName: 'Alpha',
        browserVersion: '1',
        osName: 'Windows',
        osVersion: '10',
        releaseDate: '2015-01-02T00:00:00.000Z'
      }, testCtx)),
      browsers.put(Browser.create({
        browserName: 'Alpha',
        browserVersion: '2',
        osName: 'Windows',
        osVersion: '10',
        releaseDate: '2016-01-03T00:00:00.000Z'
      }, testCtx)),
      browsers.put(Browser.create({
        browserName: 'Beta',
        browserVersion: '1',
        osName: 'Windows',
        osVersion: '10',
        releaseDate: '2016-01-04T00:00:00.000Z'
      }, testCtx)),
      browsers.put(Browser.create({
        browserName: 'Charlie',
        browserVersion: '1',
        osName: 'Windows',
        osVersion: '10',
        releaseDate: '2016-01-05T00:00:00.000Z'
      }, testCtx)),
      browsers.put(Browser.create({
        browserName: 'Alpha',
        browserVersion: '3',
        osName: 'Windows',
        osVersion: '10',
        releaseDate: '2017-01-06T00:00:00.000Z'
      }, testCtx)),
    ]).then(function(browsersArray) {
      alpha0 = browsersArray[0];
      alpha1 = browsersArray[1];
      alpha2 = browsersArray[2];
      beta = browsersArray[3];
      charlie = browsersArray[4];
      alpha3 = browsersArray[5];

      // Two APIs, shipped by Alpha, Beta, and Charlie, one removed in Alpha
      // 1, another removed in Alpha 2, one removed over a year before Beta,
      // and Charlie releases, the other removed over a year before Alpha 3
      // release.
      const ifaces = testCtx.webInterfaceDAO;
      const Junction =
          foam.lookup('org.chromium.apis.web.BrowserWebInterfaceJunction');
      const junctions = testCtx.browserWebInterfaceJunctionDAO;
      const iface1 = WebInterface.create({
        interfaceName: 'Alpha',
        apiName: 'removesIn1',
      }, testCtx);
      const iface2 = WebInterface.create({
        interfaceName: 'Alpha',
        apiName: 'removesIn2',
      }, testCtx);
      return Promise.all([
        ifaces.put(iface1),
        ifaces.put(iface2),

        // iface1 removed in Alpha 1.
        junctions.put(Junction.create({
          id: [alpha0.id, iface1.id],
          sourceId: alpha0.id,
          targetId: iface1.id,
        })),
        junctions.put(Junction.create({
          id: [beta.id, iface1.id],
          sourceId: beta.id,
          targetId: iface1.id,
        })),
        junctions.put(Junction.create({
          id: [charlie.id, iface1.id],
          sourceId: charlie.id,
          targetId: iface1.id,
        })),

        // iface2 removed in Alpha 2.
        junctions.put(Junction.create({
          id: [alpha0.id, iface2.id],
          sourceId: alpha0.id,
          targetId: iface2.id,
        })),
        junctions.put(Junction.create({
          id: [alpha1.id, iface2.id],
          sourceId: alpha1.id,
          targetId: iface2.id,
        })),
        junctions.put(Junction.create({
          id: [beta.id, iface2.id],
          sourceId: beta.id,
          targetId: iface2.id,
        })),
        junctions.put(Junction.create({
          id: [charlie.id, iface2.id],
          sourceId: charlie.id,
          targetId: iface2.id,
        })),
      ]);
    }).then(function() {
      // Setup and run aggressive removal metric calculation.
      aggressiveRemoval = AggressiveRemoval.create({
        browserDAO: testCtx.browserDAO,
        interfaceDAO: testCtx.webInterfaceDAO,
        browserApiDAO: testCtx.browserWebInterfaceJunctionDAO,
      }, testCtx);
      return aggressiveRemoval.run();
    }).then(function() {
      return aggressiveRemoval.aggressiveRemovalDAO.select();
    }).then(function(sink) {
      var array = sink.a;
      // Two data points satisfies computation constraints:
      // 1. Computed from date with at least one version from each vendor,
      // 2. Computed from date with at least two versions >1yr-old from vendor
      //    whose removals are under analysis.
      expect(array.length).toBe(2);

      // First point: Removal from Alpha 1 counted at time of Charlie release.
      var ar1 = array[0];
      expect(ar1.browserName).toBe('Alpha');
      expect(ar1.numAggressiveRemoval).toBe(1);
      // First date that all vendors have a release: Charlie release date.
      expect(ar1.date).toEqual(charlie.releaseDate);
      expect(foam.util.equals(ar1.browserOneYearAgo, alpha1)).toBe(true);
      expect(foam.util.equals(ar1.prevReleaseBrowsers.sort(), [alpha0].sort()))
          .toBe(true);
      expect(foam.util.equals(ar1.currBrowsers.sort(), [beta, charlie].sort()))
          .toBe(true);
      done();

      // Second point: Removals from Alpha 1 and Alpha 2 counted at time of
      // Alpha 3 release.
      var ar2 = array[1];
      expect(ar2.browserName).toBe('Alpha');
      // Both removals counted.
      expect(ar2.numAggressiveRemoval).toBe(2);
      // First date that all vendors have a release: Charlie release date.
      expect(ar2.date).toEqual(alpha3.releaseDate);
      expect(foam.util.equals(ar2.browserOneYearAgo, alpha2)).toBe(true);
      expect(foam.util.equals(ar2.prevReleaseBrowsers.sort(),
                              [alpha0, alpha1].sort()))
                                  .toBe(true);
      expect(foam.util.equals(ar2.currBrowsers.sort(), [beta, charlie].sort()))
          .toBe(true);
      done();
    });
  });

  it('should not count APIs that are reintroduced', function(done) {
    const Browser = testCtx.lookup('org.chromium.apis.web.Browser');
    const WebInterface = testCtx.lookup('org.chromium.apis.web.WebInterface');
    const AggressiveRemoval =
        testCtx.lookup('org.chromium.apis.web.AggressiveRemoval');
    const browsers = testCtx.browserDAO;
    let alpha1, alpha2, alpha3, beta, charlie, aggressiveRemoval;
    // Instantiate browser releases:
    // Alpha 1, 2, 3: ~1 year apart.
    // Beta, Charlie: same year as Alpha 3.
    Promise.all([
      browsers.put(Browser.create({
        browserName: 'Alpha',
        browserVersion: '1',
        osName: 'Windows',
        osVersion: '10',
        releaseDate: '2015-01-01T00:00:00.000Z'
      }, testCtx)),
      browsers.put(Browser.create({
        browserName: 'Alpha',
        browserVersion: '2',
        osName: 'Windows',
        osVersion: '10',
        releaseDate: '2016-01-02T00:00:00.000Z'
      }, testCtx)),
      browsers.put(Browser.create({
        browserName: 'Alpha',
        browserVersion: '3',
        osName: 'Windows',
        osVersion: '10',
        releaseDate: '2017-01-03T00:00:00.000Z'
      }, testCtx)),
      browsers.put(Browser.create({
        browserName: 'Beta',
        browserVersion: '1',
        osName: 'Windows',
        osVersion: '10',
        releaseDate: '2017-01-04T00:00:00.000Z'
      }, testCtx)),
      browsers.put(Browser.create({
        browserName: 'Charlie',
        browserVersion: '1',
        osName: 'Windows',
        osVersion: '10',
        releaseDate: '2017-01-05T00:00:00.000Z'
      }, testCtx)),
    ]).then(function(browsersArray) {
      alpha1 = browsersArray[0];
      alpha2 = browsersArray[1];
      alpha3 = browsersArray[2];
      beta = browsersArray[3];
      charlie = browsersArray[4];

      // One API, shipped by Alpha, Beta, and Charlie, removed in Alpha 2,
      // more than a year after Alpha 3, Beta, and Charlie releases. However,
      // Alpha re-introduced API in version 3.
      // That is, API ships in Alpha 1, Alpha 3, Beta, and Charlie.
      const ifaces = testCtx.webInterfaceDAO;
      const Junction =
          foam.lookup('org.chromium.apis.web.BrowserWebInterfaceJunction');
      const junctions = testCtx.browserWebInterfaceJunctionDAO;
      const iface = WebInterface.create({
        interfaceName: 'Alpha',
        apiName: 'removesIn2',
      }, testCtx);
      return Promise.all([
        ifaces.put(iface),
        junctions.put(Junction.create({
          id: [alpha1.id, iface.id],
          sourceId: alpha1.id,
          targetId: iface.id,
        })),
        junctions.put(Junction.create({
          id: [alpha3.id, iface.id],
          sourceId: alpha3.id,
          targetId: iface.id,
        })),
        junctions.put(Junction.create({
          id: [beta.id, iface.id],
          sourceId: beta.id,
          targetId: iface.id,
        })),
        junctions.put(Junction.create({
          id: [charlie.id, iface.id],
          sourceId: charlie.id,
          targetId: iface.id,
        })),
      ]);
    }).then(function() {
      // Setup and run aggressive removal metric calculation.
      aggressiveRemoval = AggressiveRemoval.create({
        browserDAO: testCtx.browserDAO,
        interfaceDAO: testCtx.webInterfaceDAO,
        browserApiDAO: testCtx.browserWebInterfaceJunctionDAO,
      }, testCtx);
      return aggressiveRemoval.run();
    }).then(function() {
      return aggressiveRemoval.aggressiveRemovalDAO.select();
    }).then(function(sink) {
      var array = sink.a;
      // One data point satisfies computation constraints:
      // 1. Computed from date with at least one version from each vendor,
      // 2. Computed from date with at least two versions >1yr-old from vendor
      //    whose removals are under analysis.
      expect(array.length).toBe(1);
      var ar = array[0];
      expect(ar.browserName).toBe('Alpha');
      // Removal in Alpha 2 not counted: API is later reintroduced.
      expect(ar.numAggressiveRemoval).toBe(0);
      // First date that all vendors have a release: Charlie release date.
      expect(ar.date).toEqual(charlie.releaseDate);
      expect(foam.util.equals(ar.browserOneYearAgo, alpha2)).toBe(true);
      expect(foam.util.equals(ar.prevReleaseBrowsers, [alpha1])).toBe(true);
      expect(foam.util.equals(ar.currBrowsers.sort(), [beta, charlie].sort()))
          .toBe(true);
      done();
    });
  });
});
