// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

describe('AggressiveRemoval', function() {
  function equals(a, b) {
    return foam.util.equals(a, b);
  }
  function sortedEquals(a, b) {
    function sort(array) { return array.sort(foam.util.compare); }
    return equals(sort(a), sort(b));
  }
  let Release;
  let WebInterface;
  let Junction;
  let container;
  let runner;
  beforeEach(function() {
    Release = foam.lookup('org.chromium.apis.web.Release');
    WebInterface = foam.lookup('org.chromium.apis.web.WebInterface');
    Junction = foam.lookup('org.chromium.apis.web.ReleaseWebInterfaceJunction');
    container = global.createDAOContainer();
    runner = global.createLocalRunner({
      metricComputerTypes: [
        foam.lookup('org.chromium.apis.web.MetricComputerType').AGGRESSIVE_REMOVAL,
      ],
    }, container);
  });

  it('should handle simple case', function(done) {
    const Release = foam.lookup('org.chromium.apis.web.Release');
    const WebInterface = foam.lookup('org.chromium.apis.web.WebInterface');
    const releases = container.releaseDAO;
    let alpha1, alpha2, alpha3, beta, charlie;
    // Instantiate release releases:
    // Alpha 1, 2, 3: ~1 year apart.
    // Beta, Charlie: same year as Alpha 3.
    Promise.all([
      releases.put(Release.create({
        browserName: 'Alpha',
        browserVersion: '1',
        osName: 'Windows',
        osVersion: '10',
        releaseDate: '2015-01-01T00:00:00.000Z'
      }, container)),
      releases.put(Release.create({
        browserName: 'Alpha',
        browserVersion: '2',
        osName: 'Windows',
        osVersion: '10',
        releaseDate: '2016-01-02T00:00:00.000Z'
      }, container)),
      releases.put(Release.create({
        browserName: 'Alpha',
        browserVersion: '3',
        osName: 'Windows',
        osVersion: '10',
        releaseDate: '2017-01-03T00:00:00.000Z'
      }, container)),
      releases.put(Release.create({
        browserName: 'Beta',
        browserVersion: '1',
        osName: 'Windows',
        osVersion: '10',
        releaseDate: '2017-01-04T00:00:00.000Z'
      }, container)),
      releases.put(Release.create({
        browserName: 'Charlie',
        browserVersion: '1',
        osName: 'Windows',
        osVersion: '10',
        releaseDate: '2017-01-05T00:00:00.000Z'
      }, container)),
    ]).then(function(releasesArray) {
      alpha1 = releasesArray[0];
      alpha2 = releasesArray[1];
      alpha3 = releasesArray[2];
      beta = releasesArray[3];
      charlie = releasesArray[4];

      // One API, shipped by Alpha, Beta, and Charlie, removed in Alpha 2,
      // more than a year after Alpha 3, Beta, and Charlie releases.
      // That is, API ships in Alpha 1, Beta, and Charlie.
      const ifaces = container.webInterfaceDAO;
      const Junction =
          foam.lookup('org.chromium.apis.web.ReleaseWebInterfaceJunction');
      const junctions = container.releaseWebInterfaceJunctionDAO;
      const iface = WebInterface.create({
        interfaceName: 'Alpha',
        apiName: 'removesIn2',
      }, container);
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
      return runner.run();
    }).then(function() {
      return container.browserMetricsDAO.select();
    }).then(function(sink) {
      var array = sink.array;
      // One data point satisfies computation constraints:
      // 1. Computed from date with at least one version from each browser,
      // 2. Computed from date with at least two versions >1yr-old from browser
      //    whose removals are under analysis.
      expect(array.length).toBe(1);
      var ar = array[0];
      expect(ar.browserName).toBe('Alpha');
      expect(ar.value).toBe(1);
      // First date that all browsers have a release: Charlie release date.
      expect(ar.date).toEqual(charlie.releaseDate);
      expect(equals(ar.release, alpha2)).toBe(true);
      expect(equals(ar.prevReleases, [alpha1])).toBe(true);
      expect(sortedEquals(ar.comparedReleases, [beta, charlie])).toBe(true);
      done();
    });
  });

  it('should not capture removal less than a year old', function(done) {
    const Release = foam.lookup('org.chromium.apis.web.Release');
    const WebInterface = foam.lookup('org.chromium.apis.web.WebInterface');
    const releases = container.releaseDAO;
    let alpha0, alpha1, alpha2, alpha3, beta, charlie;
    // Instantiate browser releases:
    // Alpha 0, 1, 2, 3: ~1 year apart.
    // Beta, Charlie: same year as Alpha 3.
    // Here, Alpha 2 released less than a year before Alpha 3, Beta, or Charlie.
    // Alpha 0 is introduced to hit case where there are two Alpha versions old
    // enough to compute Alpha's aggressive removals.
    Promise.all([
      releases.put(Release.create({
        browserName: 'Alpha',
        browserVersion: '0',
        osName: 'Windows',
        osVersion: '10',
        releaseDate: '2014-01-01T00:00:00.000Z'
      }, container)),
      releases.put(Release.create({
        browserName: 'Alpha',
        browserVersion: '1',
        osName: 'Windows',
        osVersion: '10',
        releaseDate: '2015-01-01T00:00:00.000Z'
      }, container)),
      releases.put(Release.create({
        browserName: 'Alpha',
        browserVersion: '2',
        osName: 'Windows',
        osVersion: '10',
        releaseDate: '2016-01-06T00:00:00.000Z'
      }, container)),
      releases.put(Release.create({
        browserName: 'Alpha',
        browserVersion: '3',
        osName: 'Windows',
        osVersion: '10',
        releaseDate: '2017-01-03T00:00:00.000Z'
      }, container)),
      releases.put(Release.create({
        browserName: 'Beta',
        browserVersion: '1',
        osName: 'Windows',
        osVersion: '10',
        releaseDate: '2017-01-04T00:00:00.000Z'
      }, container)),
      releases.put(Release.create({
        browserName: 'Charlie',
        browserVersion: '1',
        osName: 'Windows',
        osVersion: '10',
        releaseDate: '2017-01-05T00:00:00.000Z'
      }, container)),
    ]).then(function(releasesArray) {
      alpha0 = releasesArray[0];
      alpha1 = releasesArray[1];
      alpha2 = releasesArray[2];
      alpha3 = releasesArray[3];
      beta = releasesArray[4];
      charlie = releasesArray[5];

      // One API, shipped by Alpha, Beta, and Charlie, removed in Alpha 2,
      // but less than a year after Alpha 3, Beta, and Charlie releases.
      // That is, API ships in Alpha 0, Alpha 1, Beta, and Charlie.
      const ifaces = container.webInterfaceDAO;
      const Junction =
          foam.lookup('org.chromium.apis.web.ReleaseWebInterfaceJunction');
      const junctions = container.releaseWebInterfaceJunctionDAO;
      const iface = WebInterface.create({
        interfaceName: 'Alpha',
        apiName: 'removesIn2',
      }, container);
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
      return runner.run();
    }).then(function() {
      return container.browserMetricsDAO.select();
    }).then(function(sink) {
      var array = sink.array;
      // One data point satisfies computation constraints:
      // 1. Computed from date with at least one version from each browser,
      // 2. Computed from date with at least two versions >1yr-old from browser
      //    whose removals are under analysis.
      expect(array.length).toBe(1);
      var ar = array[0];
      expect(ar.browserName).toBe('Alpha');
      // Removal in Alpha 2 not counted: it's less than 1yr-old.
      expect(ar.value).toBe(0);
      // First date that all browsers have a release: Charlie release date.
      expect(ar.date).toEqual(charlie.releaseDate);
      expect(equals(ar.release, alpha1)).toBe(true);
      expect(equals(ar.prevReleases, [alpha0])).toBe(true);
      expect(sortedEquals(ar.comparedReleases, [beta, charlie])).toBe(true);
      done();
    });
  });

  it('should capture old removals', function(done) {
    const Release = foam.lookup('org.chromium.apis.web.Release');
    const WebInterface = foam.lookup('org.chromium.apis.web.WebInterface');
    const releases = container.releaseDAO;
    let alpha0, alpha1, alpha2, alpha3, beta, charlie;
    // Instantiate release releases:
    // Alpha 0, 1, 2, 3: ~1 year apart.
    // Beta, Charlie: same year as Alpha 3.
    // Alpha 0 is introduced to get two removals from different years.
    Promise.all([
      releases.put(Release.create({
        browserName: 'Alpha',
        browserVersion: '0',
        osName: 'Windows',
        osVersion: '10',
        releaseDate: '2014-01-01T00:00:00.000Z'
      }, container)),
      releases.put(Release.create({
        browserName: 'Alpha',
        browserVersion: '1',
        osName: 'Windows',
        osVersion: '10',
        releaseDate: '2015-01-02T00:00:00.000Z'
      }, container)),
      releases.put(Release.create({
        browserName: 'Alpha',
        browserVersion: '2',
        osName: 'Windows',
        osVersion: '10',
        releaseDate: '2016-01-03T00:00:00.000Z'
      }, container)),
      releases.put(Release.create({
        browserName: 'Alpha',
        browserVersion: '3',
        osName: 'Windows',
        osVersion: '10',
        releaseDate: '2017-01-04T00:00:00.000Z'
      }, container)),
      releases.put(Release.create({
        browserName: 'Beta',
        browserVersion: '1',
        osName: 'Windows',
        osVersion: '10',
        releaseDate: '2017-01-05T00:00:00.000Z'
      }, container)),
      releases.put(Release.create({
        browserName: 'Charlie',
        browserVersion: '1',
        osName: 'Windows',
        osVersion: '10',
        releaseDate: '2017-01-06T00:00:00.000Z'
      }, container)),
    ]).then(function(releasesArray) {
      alpha0 = releasesArray[0];
      alpha1 = releasesArray[1];
      alpha2 = releasesArray[2];
      alpha3 = releasesArray[3];
      beta = releasesArray[4];
      charlie = releasesArray[5];

      // Two APIs, shipped by Alpha, Beta, and Charlie, one removed in Alpha
      // 1, another removed in Alpha 2, both removed over a year before Alpha
      // 3, Beta, and Charlie releases.
      const ifaces = container.webInterfaceDAO;
      const Junction =
          foam.lookup('org.chromium.apis.web.ReleaseWebInterfaceJunction');
      const junctions = container.releaseWebInterfaceJunctionDAO;
      const iface1 = WebInterface.create({
        interfaceName: 'Alpha',
        apiName: 'removesIn1',
      }, container);
      const iface2 = WebInterface.create({
        interfaceName: 'Alpha',
        apiName: 'removesIn2',
      }, container);
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
      return runner.run();
    }).then(function() {
      return container.browserMetricsDAO.select();
    }).then(function(sink) {
      var array = sink.array;
      // One data point satisfies computation constraints:
      // 1. Computed from date with at least one version from each browser,
      // 2. Computed from date with at least two versions >1yr-old from browser
      //    whose removals are under analysis.
      expect(array.length).toBe(1);
      var ar = array[0];
      expect(ar.browserName).toBe('Alpha');
      // Both removals counted.
      expect(ar.value).toBe(2);
      // First date that all browsers have a release: Charlie release date.
      expect(ar.date).toEqual(charlie.releaseDate);
      expect(equals(ar.release, alpha2)).toBe(true);
      expect(sortedEquals(ar.prevReleases, [alpha0, alpha1])).toBe(true);
      expect(sortedEquals(ar.comparedReleases, [beta, charlie])).toBe(true);
      done();
    });
  });

  it('should capture accumulate old removals', function(done) {
    const Release = foam.lookup('org.chromium.apis.web.Release');
    const WebInterface = foam.lookup('org.chromium.apis.web.WebInterface');
    const releases = container.releaseDAO;
    let alpha0, alpha1, alpha2, alpha3, beta, charlie;
    // Instantiate release releases:
    // Alpha 0, 1, 2, 3: ~1 year apart.
    // Beta, Charlie: same year as Alpha 2.
    // Alpha 0 is introduced to get two removals from different years.
    Promise.all([
      releases.put(Release.create({
        browserName: 'Alpha',
        browserVersion: '0',
        osName: 'Windows',
        osVersion: '10',
        releaseDate: '2014-01-01T00:00:00.000Z'
      }, container)),
      releases.put(Release.create({
        browserName: 'Alpha',
        browserVersion: '1',
        osName: 'Windows',
        osVersion: '10',
        releaseDate: '2015-01-02T00:00:00.000Z'
      }, container)),
      releases.put(Release.create({
        browserName: 'Alpha',
        browserVersion: '2',
        osName: 'Windows',
        osVersion: '10',
        releaseDate: '2016-01-03T00:00:00.000Z'
      }, container)),
      releases.put(Release.create({
        browserName: 'Beta',
        browserVersion: '1',
        osName: 'Windows',
        osVersion: '10',
        releaseDate: '2016-01-04T00:00:00.000Z'
      }, container)),
      releases.put(Release.create({
        browserName: 'Charlie',
        browserVersion: '1',
        osName: 'Windows',
        osVersion: '10',
        releaseDate: '2016-01-05T00:00:00.000Z'
      }, container)),
      releases.put(Release.create({
        browserName: 'Alpha',
        browserVersion: '3',
        osName: 'Windows',
        osVersion: '10',
        releaseDate: '2017-01-06T00:00:00.000Z'
      }, container)),
    ]).then(function(releasesArray) {
      alpha0 = releasesArray[0];
      alpha1 = releasesArray[1];
      alpha2 = releasesArray[2];
      beta = releasesArray[3];
      charlie = releasesArray[4];
      alpha3 = releasesArray[5];

      // Two APIs, shipped by Alpha, Beta, and Charlie, one removed in Alpha
      // 1, another removed in Alpha 2, one removed over a year before Beta,
      // and Charlie releases, the other removed over a year before Alpha 3
      // release.
      const ifaces = container.webInterfaceDAO;
      const Junction =
          foam.lookup('org.chromium.apis.web.ReleaseWebInterfaceJunction');
      const junctions = container.releaseWebInterfaceJunctionDAO;
      const iface1 = WebInterface.create({
        interfaceName: 'Alpha',
        apiName: 'removesIn1',
      }, container);
      const iface2 = WebInterface.create({
        interfaceName: 'Alpha',
        apiName: 'removesIn2',
      }, container);
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
      return runner.run();
    }).then(function() {
      return container.browserMetricsDAO.select();
    }).then(function(sink) {
      var array = sink.array;
      // Two data points satisfies computation constraints:
      // 1. Computed from date with at least one version from each browser,
      // 2. Computed from date with at least two versions >1yr-old from browser
      //    whose removals are under analysis.
      expect(array.length).toBe(2);

      // First point: Removal from Alpha 1 counted at time of Charlie release.
      var ar1 = array[0];
      expect(ar1.browserName).toBe('Alpha');
      expect(ar1.value).toBe(1);
      // First date that all browsers have a release: Charlie release date.
      expect(ar1.date).toEqual(charlie.releaseDate);
      expect(equals(ar1.release, alpha1)).toBe(true);
      expect(sortedEquals(ar1.prevReleases, [alpha0])).toBe(true);
      expect(sortedEquals(ar1.comparedReleases, [beta, charlie])).toBe(true);
      done();

      // Second point: Removals from Alpha 1 and Alpha 2 counted at time of
      // Alpha 3 release.
      var ar2 = array[1];
      expect(ar2.browserName).toBe('Alpha');
      // Both removals counted.
      expect(ar2.value).toBe(2);
      // First date that all browsers have a release: Charlie release date.
      expect(ar2.date).toEqual(alpha3.releaseDate);
      expect(equals(ar2.release, alpha2)).toBe(true);
      expect(sortedEquals(ar2.prevReleases, [alpha0, alpha1]))
          .toBe(true);
      expect(sortedEquals(ar2.comparedReleases, [beta, charlie])).toBe(true);
      done();
    });
  });

  it('should not count APIs that are reintroduced', function(done) {
    const Release = foam.lookup('org.chromium.apis.web.Release');
    const WebInterface = foam.lookup('org.chromium.apis.web.WebInterface');
    const releases = container.releaseDAO;
    let alpha1, alpha2, alpha3, beta, charlie;
    // Instantiate release releases:
    // Alpha 1, 2, 3: ~1 year apart.
    // Beta, Charlie: same year as Alpha 3.
    Promise.all([
      releases.put(Release.create({
        browserName: 'Alpha',
        browserVersion: '1',
        osName: 'Windows',
        osVersion: '10',
        releaseDate: '2015-01-01T00:00:00.000Z'
      }, container)),
      releases.put(Release.create({
        browserName: 'Alpha',
        browserVersion: '2',
        osName: 'Windows',
        osVersion: '10',
        releaseDate: '2016-01-02T00:00:00.000Z'
      }, container)),
      releases.put(Release.create({
        browserName: 'Alpha',
        browserVersion: '3',
        osName: 'Windows',
        osVersion: '10',
        releaseDate: '2017-01-03T00:00:00.000Z'
      }, container)),
      releases.put(Release.create({
        browserName: 'Beta',
        browserVersion: '1',
        osName: 'Windows',
        osVersion: '10',
        releaseDate: '2017-01-04T00:00:00.000Z'
      }, container)),
      releases.put(Release.create({
        browserName: 'Charlie',
        browserVersion: '1',
        osName: 'Windows',
        osVersion: '10',
        releaseDate: '2017-01-05T00:00:00.000Z'
      }, container)),
    ]).then(function(releasesArray) {
      alpha1 = releasesArray[0];
      alpha2 = releasesArray[1];
      alpha3 = releasesArray[2];
      beta = releasesArray[3];
      charlie = releasesArray[4];

      // One API, shipped by Alpha, Beta, and Charlie, removed in Alpha 2,
      // more than a year after Alpha 3, Beta, and Charlie releases. However,
      // Alpha re-introduced API in version 3.
      // That is, API ships in Alpha 1, Alpha 3, Beta, and Charlie.
      const ifaces = container.webInterfaceDAO;
      const Junction =
          foam.lookup('org.chromium.apis.web.ReleaseWebInterfaceJunction');
      const junctions = container.releaseWebInterfaceJunctionDAO;
      const iface = WebInterface.create({
        interfaceName: 'Alpha',
        apiName: 'removesIn2',
      }, container);
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
      return runner.run();
    }).then(function() {
      return container.browserMetricsDAO.select();
    }).then(function(sink) {
      var array = sink.array;
      // One data point satisfies computation constraints:
      // 1. Computed from date with at least one version from each browser,
      // 2. Computed from date with at least two versions >1yr-old from browser
      //    whose removals are under analysis.
      expect(array.length).toBe(1);
      var ar = array[0];
      expect(ar.browserName).toBe('Alpha');
      // Removal in Alpha 2 not counted: API is later reintroduced.
      expect(ar.value).toBe(0);
      // First date that all browsers have a release: Charlie release date.
      expect(ar.date).toEqual(charlie.releaseDate);
      expect(equals(ar.release, alpha2)).toBe(true);
      expect(sortedEquals(ar.prevReleases, [alpha1])).toBe(true);
      expect(sortedEquals(ar.comparedReleases, [beta, charlie])).toBe(true);
      done();
    });
  });
});
