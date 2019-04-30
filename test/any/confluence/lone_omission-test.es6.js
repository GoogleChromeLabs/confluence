// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

describe('LoneOmission', function() {
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
  let BrowserMetricDataType;
  let BrowserMetricData;
  let Release;
  let container;
  let gen;
  let runner;
  let releases;
  let compatData;
  const date1 = '2015-01-01T00:00:00.000Z';
  const date2 = '2016-02-01T00:00:00.000Z';
  const date2_1 = '2016-03-01T00:00:00.000Z';
  const date3 = '2017-04-01T00:00:00.000Z';
  function mkData(value, date, release, prevReleases, comparedReleases) {
    return BrowserMetricData.create({
      type: BrowserMetricDataType.LONE_OMISSION,
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
    CompatData = global.defineGeneratedCompatData(gen, releaseSpecs);

    Release = foam.lookup('org.chromium.apis.web.Release');
    BrowserMetricDataType =
      foam.lookup('org.chromium.apis.web.BrowserMetricDataType');
    BrowserMetricData =
      foam.lookup('org.chromium.apis.web.BrowserMetricData');
    container = global.createDAOContainer();
    runner = global.createLocalRunner({
      metricComputerTypes: [
        foam.lookup('org.chromium.apis.web.MetricComputerType').LONE_OMISSION,
      ],
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

    return Promise.all([
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
      expect(sortedEquals(sink.array, [
        // Alpha fails to ship BC.
        mkData(1, date1, alpha, [], [beta, charlie]),
        // Beta fails to ship AC.
        mkData(1, date1, beta, [], [alpha, charlie]),
        // Charlie fails to ship AB.
        mkData(1, date1, charlie, [], [alpha, beta]),
      ])).toBe(true);
    }).then(done, done.fail);
  });

  it('should exclude API if this browser ever shipped it', function(done) {
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
        interfaceName: 'ABC',
        apiName: 'all',
        [gen.propertyNameFromRelease(alpha1)]: true,
        [gen.propertyNameFromRelease(alpha2)]: true,
        [gen.propertyNameFromRelease(beta1)]: true,
      })),
    ]).then(function() {
      return runner.run();
    }).then(function() {
      return container.browserMetricsDAO.select();
    }).then(function(sink) {
      expect(sortedEquals(sink.array, [
        mkData(0, date1, alpha1, [], [beta1]),
        mkData(0, date3, alpha2, [alpha1], [beta2]),
        mkData(0, date1, beta1, [], [alpha1]),
        mkData(0, date3, beta2, [beta1], [alpha2]),
      ])).toBe(true);
    }).then(done, done.fail);
  });

  it('should exclude APIs not shipped by even one compared release', function(done) {
    // Use date2 and date2_1 to ensure two versions during grace period.
    const [alpha2, alpha2_1, beta2, beta2_1, charlie2, charlie2_1] = init([
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
        browserName: 'Charlie',
        browserVersion: '2',
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
      releases.put(alpha2),
      releases.put(alpha2_1),
      releases.put(beta2),
      releases.put(beta2_1),
      releases.put(charlie2),
      releases.put(charlie2_1),
      compatData.put(CompatData.create({
        interfaceName: 'ABC',
        apiName: 'all',
        [gen.propertyNameFromRelease(beta2)]: true,
        [gen.propertyNameFromRelease(beta2_1)]: true,
        [gen.propertyNameFromRelease(charlie2_1)]: true,
      })),
    ]).then(function() {
      return runner.run();
    }).then(function() {
      return container.browserMetricsDAO.select();
    }).then(function(sink) {
      // Expected data:
      //
      // mkData(0, date2, alpha2, [], [beta2, charlie2]),
      // mkData(0, date2_1, alpha2_1, [alpha2],
      //        sort([beta2, beta2_1, charlie2, charlie2_1])),
      // mkData(0, date2, beta2, [], [alpha2, charlie2]),
      // mkData(0, date2_1, beta2_1, [beta2],
      //        sort([alpha2, alpha2_1, charlie2, charlie2_1])),
      // mkData(0, date2, charlie2, [], [alpha2, beta2]),
      // mkData(0, date2_1, charlie2_1, [charlie2], [alpha2, alpha2_1]),
      //        sort([beta2, beta2_1, beta2, beta2_1])),
      //
      // The important bit: alpha2_1 does not register a lone omission, even
      // though beta2_1 and charlie2_1 shipped an API that alpha2_1 does not.
      // This is because charlie2, which is within the grace period, did not
      // ship the API; lone omission only registers when ALL releases of other
      // browsers within the grace period shipped the API.
      expect(sink.array[1].release.id).toBe('Alpha_2.1_Windows_10');
      expect(sink.array[1].value).toBe(0);
    }).then(done, done.fail);
  });
});
