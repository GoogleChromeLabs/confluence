// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

describe('BrowserSpecific', function() {
  function equals(a, b) {
    return foam.util.equals(a, b);
  }
  function sort(array) {
    return array.sort(foam.util.compare);
  }
  function sortedEquals(a, b) {
    return equals(sort(a), sort(b));
  }
  let CompatData;
  let Release;
  let BrowserMetricDataType;
  let BrowserMetricData;
  let compatData;
  let container;
  let gen;
  let runner;
  let releases;
  const date1 = '2015-01-01T00:00:00.000Z';
  const date2 = '2016-02-01T00:00:00.000Z';
  const date2_1 = '2016-03-01T00:00:00.000Z';
  const date3 = '2017-04-01T00:00:00.000Z';

  function mkData(value, date, release, prevReleases, comparedReleases) {
    return BrowserMetricData.create({
      type: BrowserMetricDataType.BROWSER_SPECIFIC,
      browserName: release.browserName,
      value,
      date,
      release,
      prevReleases,
      comparedReleases,
    });
  }

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
    BrowserMetricData = foam.lookup('org.chromium.apis.web.BrowserMetricData');
    BrowserMetricDataType =
        foam.lookup('org.chromium.apis.web.BrowserMetricDataType');

    container = global.createDAOContainer();
    runner = global.createLocalRunner({
      metricComputerTypes: [BrowserMetricDataType.BROWSER_SPECIFIC],
    }, container);
    releases = container.releaseDAO;
    compatData = container.compatDAO;

    return releaseSpecs.map((rs) => Release.create(rs, container));
  };

  it('should handle simple case', function(done) {
    const [alpha, beta, charlie] = init([
      {
        browserName: 'Alpha',
        browserVersion: '1',
        osName: 'Windows',
        osVersion: '10',
        releaseDate: date1,
      },
      {
        browserName: 'Beta',
        browserVersion: '1',
        osName: 'Windows',
        osVersion: '10',
        releaseDate: date1,
      },
      {
        browserName: 'Charlie',
        browserVersion: '1',
        osName: 'Windows',
        osVersion: '10',
        releaseDate: date1,
      },
    ]);
    Promise.all([
      releases.put(alpha),
      releases.put(beta),
      releases.put(charlie),
      compatData.put(CompatData.create({
        interfaceName: 'B',
        apiName: 'only',
        [gen.propertyNameFromRelease(beta)]: true,
      })),
      compatData.put(CompatData.create({
        interfaceName: 'C',
        apiName: 'only1',
        [gen.propertyNameFromRelease(charlie)]: true,
      })),
      compatData.put(CompatData.create({
        interfaceName: 'C',
        apiName: 'only2',
        [gen.propertyNameFromRelease(charlie)]: true,
      })),
      compatData.put(CompatData.create({
        interfaceName: 'AB',
        apiName: 'only',
        [gen.propertyNameFromRelease(alpha)]: true,
        [gen.propertyNameFromRelease(beta)]: true,
      })),
      compatData.put(CompatData.create({
        interfaceName: 'AC',
        apiName: 'only',
        [gen.propertyNameFromRelease(alpha)]: true,
        [gen.propertyNameFromRelease(charlie)]: true,
      })),
      compatData.put(CompatData.create({
        interfaceName: 'BC',
        apiName: 'only',
        [gen.propertyNameFromRelease(beta)]: true,
        [gen.propertyNameFromRelease(charlie)]: true,
      })),
      compatData.put(CompatData.create({
        interfaceName: 'ABC',
        apiName: 'all',
        [gen.propertyNameFromRelease(alpha)]: true,
        [gen.propertyNameFromRelease(beta)]: true,
        [gen.propertyNameFromRelease(charlie)]: true,
      })),
    ]).then(function() {
      return runner.run();
    }).then(function() {
      return container.browserMetricsDAO.select();
    }).then(function(sink) {
      expect(sortedEquals(
          sink.array,
          [
            mkData(0, date1, alpha, [], [beta, charlie]),
            mkData(1, date1, beta, [], [alpha, charlie]),
            mkData(2, date1, charlie, [], [alpha, beta]),
          ])).toBe(true);
    }).then(done, done.fail);
  });

  it('should handle old removals from other browsers', function(done) {
    // Use date1 and date3 to ensure that version 2 is more than a year after
    // version 1.
    const [alpha1, alpha2, beta1, beta2] = init([
      {
        browserName: 'Alpha',
        browserVersion: '1',
        osName: 'Windows',
        osVersion: '10',
        releaseDate: date1,
      },
      {
        browserName: 'Alpha',
        browserVersion: '2',
        osName: 'Windows',
        osVersion: '10',
        releaseDate: date3,
      },
      {
        browserName: 'Beta',
        browserVersion: '1',
        osName: 'Windows',
        osVersion: '10',
        releaseDate: date1,
      },
      {
        browserName: 'Beta',
        browserVersion: '2',
        osName: 'Windows',
        osVersion: '10',
        releaseDate: date3,
      },
    ]);


    Promise.all([
      releases.put(alpha1),
      releases.put(alpha2),
      releases.put(beta1),
      releases.put(beta2),
      compatData.put(CompatData.create({
        interfaceName: 'Removed',
        apiName: 'inB2',
        [gen.propertyNameFromRelease(alpha1)]: true,
        [gen.propertyNameFromRelease(alpha2)]: true,
        [gen.propertyNameFromRelease(beta1)]: true,
      })),
    ]).then(function() {
      return runner.run();
    }).then(function() {
      return container.browserMetricsDAO.select();
    }).then(function(sink) {
      expect(sortedEquals(
          sink.array,
          [
            mkData(0, date1, alpha1, [], [beta1]),
            mkData(1, date3, alpha2, [], [beta2]),
            mkData(0, date1, beta1, [], [alpha1]),
            mkData(0, date3, beta2, [], [alpha2]),
          ])).toBe(true);
    }).then(done, done.fail);
  });

  it('should exclude additions during grace period', function(done) {
    // Use date2 and date2_1 to ensure two versions during grace period.
    const [alpha2, alpha2_1, alpha3, beta2, beta2_1, beta3] = init([
      {
        browserName: 'Alpha',
        browserVersion: '2',
        osName: 'Windows',
        osVersion: '10',
        releaseDate: date2,
      },
      {
        browserName: 'Alpha',
        browserVersion: '2.1',
        osName: 'Windows',
        osVersion: '10',
        releaseDate: date2_1,
      },
      {
        browserName: 'Alpha',
        browserVersion: '3',
        osName: 'Windows',
        osVersion: '10',
        releaseDate: date3,
      },
      {
        browserName: 'Beta',
        browserVersion: '2',
        osName: 'Windows',
        osVersion: '10',
        releaseDate: date2,
      },
      {
        browserName: 'Beta',
        browserVersion: '2.1',
        osName: 'Windows',
        osVersion: '10',
        releaseDate: date2_1,
      },
      {
        browserName: 'Beta',
        browserVersion: '3',
        osName: 'Windows',
        osVersion: '10',
        releaseDate: date3,
      },
    ]);


    Promise.all([
      releases.put(alpha2),
      releases.put(alpha2_1),
      releases.put(alpha3),
      releases.put(beta2),
      releases.put(beta2_1),
      releases.put(beta3),
      compatData.put(CompatData.create({
        interfaceName: 'Added',
        apiName: 'inA2_1',
        [gen.propertyNameFromRelease(alpha2_1)]: true,
        [gen.propertyNameFromRelease(alpha3)]: true,
      })),
    ]).then(function() {
      return runner.run();
    }).then(function() {
      return container.browserMetricsDAO.select();
    }).then(function(sink) {
      // Expected data:
      //
      // mkData(0, date2, alpha2, [], [beta2]),
      // mkData(0, date2_1, alpha2_1, [alpha2], [beta2, beta2_1]),
      // mkData(1, date3, alpha3, [alpha2, alpha2_1],
      //        [beta2, beta2_1, beta3]),
      // mkData(0, date2, beta2, [], [alpha2]),
      // mkData(0, date2_1, beta2_1, [beta2], [alpha2, alpha2_1]),
      // mkData(0, date3, beta3, [beta2, beta2_1],
      //        [alpha2, alpha2_1, alpha3]),
      //
      // The important bit: Adding interface to alpha2_1 didn't count as
      // browser-specific because alpha2 didn't ship it, and alpha2 is within
      // the grace period. However, it does count when it's still shipping in
      // alpha3 (which has alpha2_1, but not alpha2, in its grace period).
      expect(sink.array[1].release.id).toBe('Alpha_2.1_Windows_10');
      expect(sink.array[1].value).toBe(0);
      expect(sink.array[2].release.id).toBe('Alpha_3_Windows_10');
      expect(sink.array[2].value).toBe(1);
    }).then(done, done.fail);
  });

  it('should exclude anything when just one other browser shipped during grace period', function(done) {
    // Use date2 and date2 to ensure two versions during grace period.
    const [alpha2_1, beta2, beta2_1, charlie2, charlie2_1] = init([
      {
        browserName: 'Alpha',
        browserVersion: '2.1',
        osName: 'Windows',
        osVersion: '10',
        releaseDate: date2_1,
      },
      {
        browserName: 'Beta',
        browserVersion: '2.0',
        osName: 'Windows',
        osVersion: '10',
        releaseDate: date2,
      },
      {
        browserName: 'Beta',
        browserVersion: '2.1',
        osName: 'Windows',
        osVersion: '10',
        releaseDate: date2_1,
      },
      {
        browserName: 'Charlie',
        browserVersion: '2.0',
        osName: 'Windows',
        osVersion: '10',
        releaseDate: date2,
      },
      {
        browserName: 'Charlie',
        browserVersion: '2.1',
        osName: 'Windows',
        osVersion: '10',
        releaseDate: date2_1,
      },
    ]);

    Promise.all([
      releases.put(alpha2_1),
      releases.put(beta2),
      releases.put(beta2_1),
      releases.put(charlie2),
      releases.put(charlie2_1),
      compatData.put(CompatData.create({
        interfaceName: 'Added',
        apiName: 'inA2_1AndCharlie2',
        [gen.propertyNameFromRelease(alpha2_1)]: true,
        [gen.propertyNameFromRelease(charlie2)]: true,
      })),
    ]).then(function() {
      return runner.run();
    }).then(function() {
      return container.browserMetricsDAO.select();
    }).then(function(sink) {
      // Expected data:
      //
      // mkData(0, date2_1, alpha2_1, [],
      //        [beta2, beta2_1, charlie2, charlie2_1]),
      // mkData(0, date2_1, beta2_1, [beta2], [alpha2_1, charlie2, charlie2_1])
      // mkData(0, date2_1, charlie2_1, [charlie2], [alpha2_1, beta2, beta2_1])
      //
      // Note: No version 2 data because metric only computed when a release
      // of each browser can be found. See inner callback logic in
      // MetricComputerRunner.run().
      //
      // The important bit: Adding interface to alpha3 didn't count as
      // browser-specific because charlie2 shipped it (even though neither
      // charlie3, nor any version of beta shipped it).
      expect(sink.array[0].release.id).toBe('Alpha_2.1_Windows_10');
      expect(sink.array[0].value).toBe(0);
    }).then(done, done.fail);
  });
});
