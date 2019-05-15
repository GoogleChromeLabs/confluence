// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

describe('LoneRemoval', function() {
  function equals(a, b) {
    return foam.util.equals(a, b);
  }
  function sortedEquals(a, b) {
    function sort(array) {
      return array.sort(foam.util.compare);
    }
    return equals(sort(a), sort(b));
  }
  let CompatData;
  let Release;
  let compatData;
  let container;
  let gen;
  let releases;
  let runner;

  beforeEach(() => {
    gen =
        foam.lookup('org.chromium.apis.web.AbstractCompatClassGenerator')
            .create();
  });

  const init = (releaseSpecs) => {
    // Register custom CompatData before looking up classes and instantiating
    // instances.
    CompatData = global.defineGeneratedCompatData(gen, releaseSpecs);

    Release = foam.lookup('org.chromium.apis.web.Release');

    container = global.createDAOContainer();
    runner = global.createLocalRunner({
      metricComputerTypes: [
        foam.lookup('org.chromium.apis.web.MetricComputerType').LONE_REMOVAL,
      ],
    }, container);
    releases = container.releaseDAO;
    compatData = container.compatDAO;

    return releaseSpecs.map((rs) => Release.create(rs, container));
  };

  it('should handle simple case', function(done) {
    const [alpha1, alpha2, alpha3, beta, charlie] = init([
      {
        browserName: 'Alpha',
        browserVersion: '1',
        osName: 'Windows',
        osVersion: '10',
        releaseDate: '2015-01-01T00:00:00.000Z',
      },
      {
        browserName: 'Alpha',
        browserVersion: '2',
        osName: 'Windows',
        osVersion: '10',
        releaseDate: '2016-01-02T00:00:00.000Z',
      },
      {
        browserName: 'Alpha',
        browserVersion: '3',
        osName: 'Windows',
        osVersion: '10',
        releaseDate: '2017-01-03T00:00:00.000Z',
      },
      {
        browserName: 'Beta',
        browserVersion: '1',
        osName: 'Windows',
        osVersion: '10',
        releaseDate: '2017-01-04T00:00:00.000Z',
      },
      {
        browserName: 'Charlie',
        browserVersion: '1',
        osName: 'Windows',
        osVersion: '10',
        releaseDate: '2017-01-05T00:00:00.000Z',
      },
    ]);

    return Promise.all([
      releases.put(alpha1),
      releases.put(alpha2),
      releases.put(alpha3),
      releases.put(beta),
      releases.put(charlie),
      compatData.put(CompatData.create({
        interfaceName: 'Alpha',
        apiName: 'removesIn2',
        [gen.propertyNameFromRelease(alpha1)]: true,
        [gen.propertyNameFromRelease(beta)]: true,
        [gen.propertyNameFromRelease(charlie)]: true,
      })),
    ]).then(function() {
      return runner.run();
    }).then(function() {
      return container.browserMetricsDAO.select();
    }).then(function(sink) {
      const array = sink.array;
      // One data point satisfies computation constraints:
      // 1. Computed from date with at least one version from each browser,
      // 2. Computed from date with at least two versions >1yr-old from browser
      //    whose removals are under analysis.
      expect(array.length).toBe(1);
      const ar = array[0];
      expect(ar.browserName).toBe('Alpha');
      expect(ar.value).toBe(1);
      // First date that all browsers have a release: Charlie release date.
      expect(ar.date).toEqual(charlie.releaseDate);
      expect(equals(ar.release, alpha2)).toBe(true);
      expect(equals(ar.prevReleases, [alpha1])).toBe(true);
      expect(sortedEquals(ar.comparedReleases, [beta, charlie])).toBe(true);
    }).then(done, done.fail);
  });

  it('should not capture removal less than a year old', function(done) {
    // Instantiate browser releases:
    // Alpha 0, 1, 2, 3: ~1 year apart.
    // Beta, Charlie: same year as Alpha 3.
    // Here, Alpha 2 released less than a year before Alpha 3, Beta, or Charlie.
    // Alpha 0 is introduced to hit case where there are two Alpha versions old
    // enough to compute Alpha's lone removals.
    const [alpha0, alpha1, alpha2, alpha3, beta, charlie] = init([
      {
        browserName: 'Alpha',
        browserVersion: '0',
        osName: 'Windows',
        osVersion: '10',
        releaseDate: '2014-01-01T00:00:00.000Z',
      },
      {
        browserName: 'Alpha',
        browserVersion: '1',
        osName: 'Windows',
        osVersion: '10',
        releaseDate: '2015-01-01T00:00:00.000Z',
      },
      {
        browserName: 'Alpha',
        browserVersion: '2',
        osName: 'Windows',
        osVersion: '10',
        releaseDate: '2016-01-06T00:00:00.000Z',
      },
      {
        browserName: 'Alpha',
        browserVersion: '3',
        osName: 'Windows',
        osVersion: '10',
        releaseDate: '2017-01-03T00:00:00.000Z',
      },
      {
        browserName: 'Beta',
        browserVersion: '1',
        osName: 'Windows',
        osVersion: '10',
        releaseDate: '2017-01-04T00:00:00.000Z',
      },
      {
        browserName: 'Charlie',
        browserVersion: '1',
        osName: 'Windows',
        osVersion: '10',
        releaseDate: '2017-01-05T00:00:00.000Z',
      },
    ]);

    return Promise.all([
      releases.put(alpha0),
      releases.put(alpha1),
      releases.put(alpha2),
      releases.put(alpha3),
      releases.put(beta),
      releases.put(charlie),
      compatData.put(CompatData.create({
        interfaceName: 'Alpha',
        apiName: 'removesIn2',
        [gen.propertyNameFromRelease(alpha0)]: true,
        [gen.propertyNameFromRelease(alpha1)]: true,
        [gen.propertyNameFromRelease(beta)]: true,
        [gen.propertyNameFromRelease(charlie)]: true,
      })),
    ]).then(function() {
      return runner.run();
    }).then(function() {
      return container.browserMetricsDAO.select();
    }).then(function(sink) {
      const array = sink.array;
      // One data point satisfies computation constraints:
      // 1. Computed from date with at least one version from each browser,
      // 2. Computed from date with at least two versions >1yr-old from browser
      //    whose removals are under analysis.
      expect(array.length).toBe(1);
      const ar = array[0];
      expect(ar.browserName).toBe('Alpha');
      // Removal in Alpha 2 not counted: it's less than 1yr-old.
      expect(ar.value).toBe(0);
      // First date that all browsers have a release: Charlie release date.
      expect(ar.date).toEqual(charlie.releaseDate);
      expect(equals(ar.release, alpha1)).toBe(true);
      expect(equals(ar.prevReleases, [alpha0])).toBe(true);
      expect(sortedEquals(ar.comparedReleases, [beta, charlie])).toBe(true);
    }).then(done, done.fail);
  });

  it('should capture old removals', function(done) {
    const [alpha0, alpha1, alpha2, alpha3, beta, charlie] = init([
      {
        browserName: 'Alpha',
        browserVersion: '0',
        osName: 'Windows',
        osVersion: '10',
        releaseDate: '2014-01-01T00:00:00.000Z',
      },
      {
        browserName: 'Alpha',
        browserVersion: '1',
        osName: 'Windows',
        osVersion: '10',
        releaseDate: '2015-01-02T00:00:00.000Z',
      },
      {
        browserName: 'Alpha',
        browserVersion: '2',
        osName: 'Windows',
        osVersion: '10',
        releaseDate: '2016-01-03T00:00:00.000Z',
      },
      {
        browserName: 'Alpha',
        browserVersion: '3',
        osName: 'Windows',
        osVersion: '10',
        releaseDate: '2017-01-04T00:00:00.000Z',
      },
      {
        browserName: 'Beta',
        browserVersion: '1',
        osName: 'Windows',
        osVersion: '10',
        releaseDate: '2017-01-05T00:00:00.000Z',
      },
      {
        browserName: 'Charlie',
        browserVersion: '1',
        osName: 'Windows',
        osVersion: '10',
        releaseDate: '2017-01-06T00:00:00.000Z',
      },
    ]);

    // Two APIs, shipped by Alpha, Beta, and Charlie, one removed in Alpha
    // 1, another removed in Alpha 2, both removed over a year before Alpha
    // 3, Beta, and Charlie releases.
    Promise.all([
      releases.put(alpha0),
      releases.put(alpha1),
      releases.put(alpha2),
      releases.put(alpha3),
      releases.put(beta),
      releases.put(charlie),
      compatData.put(CompatData.create({
        interfaceName: 'Alpha',
        apiName: 'removesIn1',
        [gen.propertyNameFromRelease(alpha0)]: true,
        [gen.propertyNameFromRelease(beta)]: true,
        [gen.propertyNameFromRelease(charlie)]: true,
      })),
      compatData.put(CompatData.create({
        interfaceName: 'Alpha',
        apiName: 'removesIn2',
        [gen.propertyNameFromRelease(alpha0)]: true,
        [gen.propertyNameFromRelease(alpha1)]: true,
        [gen.propertyNameFromRelease(beta)]: true,
        [gen.propertyNameFromRelease(charlie)]: true,
      })),
    ]).then(function() {
      return runner.run();
    }).then(function() {
      return container.browserMetricsDAO.select();
    }).then(function(sink) {
      const array = sink.array;
      // One data point satisfies computation constraints:
      // 1. Computed from date with at least one version from each browser,
      // 2. Computed from date with at least two versions >1yr-old from browser
      //    whose removals are under analysis.
      expect(array.length).toBe(1);
      const ar = array[0];
      expect(ar.browserName).toBe('Alpha');
      // Both removals counted.
      expect(ar.value).toBe(2);
      // First date that all browsers have a release: Charlie release date.
      expect(ar.date).toEqual(charlie.releaseDate);
      expect(equals(ar.release, alpha2)).toBe(true);
      expect(sortedEquals(ar.prevReleases, [alpha0, alpha1])).toBe(true);
      expect(sortedEquals(ar.comparedReleases, [beta, charlie])).toBe(true);
    }).then(done, done.fail);
  });

  it('should capture accumulate old removals', function(done) {
    // Instantiate releases:
    // Alpha 0, 1, 2, 3: ~1 year apart.
    // Beta, Charlie: same year as Alpha 2.
    // Alpha 0 is introduced to get two removals from different years.
    const [alpha0, alpha1, alpha2, alpha3, beta, charlie] = init([
      {
        browserName: 'Alpha',
        browserVersion: '0',
        osName: 'Windows',
        osVersion: '10',
        releaseDate: '2014-01-01T00:00:00.000Z',
      },
      {
        browserName: 'Alpha',
        browserVersion: '1',
        osName: 'Windows',
        osVersion: '10',
        releaseDate: '2015-01-02T00:00:00.000Z',
      },
      {
        browserName: 'Alpha',
        browserVersion: '2',
        osName: 'Windows',
        osVersion: '10',
        releaseDate: '2016-01-03T00:00:00.000Z',
      },
      {
        browserName: 'Alpha',
        browserVersion: '3',
        osName: 'Windows',
        osVersion: '10',
        releaseDate: '2017-01-06T00:00:00.000Z',
      },
      {
        browserName: 'Beta',
        browserVersion: '1',
        osName: 'Windows',
        osVersion: '10',
        releaseDate: '2016-01-04T00:00:00.000Z',
      },
      {
        browserName: 'Charlie',
        browserVersion: '1',
        osName: 'Windows',
        osVersion: '10',
        releaseDate: '2016-01-05T00:00:00.000Z',
      },
    ]);

    // Two APIs, shipped by Alpha, Beta, and Charlie, one removed in Alpha
    // 1, another removed in Alpha 2, one removed over a year before Beta,
    // and Charlie releases, the other removed over a year before Alpha 3
    // release.
    Promise.all([
      releases.put(alpha0),
      releases.put(alpha1),
      releases.put(alpha2),
      releases.put(alpha3),
      releases.put(beta),
      releases.put(charlie),
      compatData.put(CompatData.create({
        interfaceName: 'Alpha',
        apiName: 'removesIn1',
        [gen.propertyNameFromRelease(alpha0)]: true,
        [gen.propertyNameFromRelease(beta)]: true,
        [gen.propertyNameFromRelease(charlie)]: true,
      })),
      compatData.put(CompatData.create({
        interfaceName: 'Alpha',
        apiName: 'removesIn2',
        [gen.propertyNameFromRelease(alpha0)]: true,
        [gen.propertyNameFromRelease(alpha1)]: true,
        [gen.propertyNameFromRelease(beta)]: true,
        [gen.propertyNameFromRelease(charlie)]: true,
      })),
    ]).then(function() {
      return runner.run();
    }).then(function() {
      return container.browserMetricsDAO.select();
    }).then(function(sink) {
      const array = sink.array;
      // Two data points satisfies computation constraints:
      // 1. Computed from date with at least one version from each browser,
      // 2. Computed from date with at least two versions >1yr-old from browser
      //    whose removals are under analysis.
      expect(array.length).toBe(2);

      // First point: Removal from Alpha 1 counted at time of Charlie release.
      const ar1 = array[0];
      expect(ar1.browserName).toBe('Alpha');
      expect(ar1.value).toBe(1);
      // First date that all browsers have a release: Charlie release date.
      expect(ar1.date).toEqual(charlie.releaseDate);
      expect(equals(ar1.release, alpha1)).toBe(true);
      expect(sortedEquals(ar1.prevReleases, [alpha0])).toBe(true);
      expect(sortedEquals(ar1.comparedReleases, [beta, charlie])).toBe(true);

      // Second point: Removals from Alpha 1 and Alpha 2 counted at time of
      // Alpha 3 release.
      const ar2 = array[1];
      expect(ar2.browserName).toBe('Alpha');
      // Both removals counted.
      expect(ar2.value).toBe(2);
      // First date that all browsers have a release: Charlie release date.
      expect(ar2.date).toEqual(alpha3.releaseDate);
      expect(equals(ar2.release, alpha2)).toBe(true);
      expect(sortedEquals(ar2.prevReleases, [alpha0, alpha1]))
          .toBe(true);
      expect(sortedEquals(ar2.comparedReleases, [beta, charlie])).toBe(true);
    }).then(done, done.fail);
  });

  it('should not count APIs that are reintroduced', function(done) {
    // Instantiate releases:
    // Alpha 1, 2, 3: ~1 year apart.
    // Beta, Charlie: same year as Alpha 3.
    const [alpha1, alpha2, alpha3, beta, charlie] = init([
      {
        browserName: 'Alpha',
        browserVersion: '1',
        osName: 'Windows',
        osVersion: '10',
        releaseDate: '2015-01-01T00:00:00.000Z',
      },
      {
        browserName: 'Alpha',
        browserVersion: '2',
        osName: 'Windows',
        osVersion: '10',
        releaseDate: '2016-01-02T00:00:00.000Z',
      },
      {
        browserName: 'Alpha',
        browserVersion: '3',
        osName: 'Windows',
        osVersion: '10',
        releaseDate: '2017-01-03T00:00:00.000Z',
      },
      {
        browserName: 'Beta',
        browserVersion: '1',
        osName: 'Windows',
        osVersion: '10',
        releaseDate: '2017-01-04T00:00:00.000Z',
      },
      {
        browserName: 'Charlie',
        browserVersion: '1',
        osName: 'Windows',
        osVersion: '10',
        releaseDate: '2017-01-05T00:00:00.000Z',
      },
    ]);

    Promise.all([
      releases.put(alpha1),
      releases.put(alpha2),
      releases.put(alpha3),
      releases.put(beta),
      releases.put(charlie),
      compatData.put(CompatData.create({
        interfaceName: 'Alpha',
        apiName: 'removesIn2',
        [gen.propertyNameFromRelease(alpha1)]: true,
        [gen.propertyNameFromRelease(alpha3)]: true,
        [gen.propertyNameFromRelease(beta)]: true,
        [gen.propertyNameFromRelease(charlie)]: true,
      })),
    ]).then(function() {
      return runner.run();
    }).then(function() {
      return container.browserMetricsDAO.select();
    }).then(function(sink) {
      const array = sink.array;
      // One data point satisfies computation constraints:
      // 1. Computed from date with at least one version from each browser,
      // 2. Computed from date with at least two versions >1yr-old from browser
      //    whose removals are under analysis.
      expect(array.length).toBe(1);
      const ar = array[0];
      expect(ar.browserName).toBe('Alpha');
      // Removal in Alpha 2 not counted: API is later reintroduced.
      expect(ar.value).toBe(0);
      // First date that all browsers have a release: Charlie release date.
      expect(ar.date).toEqual(charlie.releaseDate);
      expect(equals(ar.release, alpha2)).toBe(true);
      expect(sortedEquals(ar.prevReleases, [alpha1])).toBe(true);
      expect(sortedEquals(ar.comparedReleases, [beta, charlie])).toBe(true);
    }).then(done, done.fail);
  });
});
