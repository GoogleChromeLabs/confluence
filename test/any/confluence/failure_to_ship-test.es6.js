// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

describe('FailureToShip', function() {
  function equals(a, b) {
    return foam.util.equals(a, b);
  }
  function sort(array) { return array.sort(foam.util.compare); }
  function sortedEquals(a, b) {
    return equals(sort(a), sort(b));
  }
  let Release;
  let WebInterface;
  let Junction;
  let FailureToShip;
  let BrowserMetricDataType;
  let BrowserMetricData;
  let container;
  let runner;
  let releases;
  let ifaces;
  let junctions;
  const date1 = '2015-01-01T00:00:00.000Z';
  const date2 = '2016-02-01T00:00:00.000Z';
  const date2_1 = '2016-03-01T00:00:00.000Z';
  const date3 = '2017-04-01T00:00:00.000Z';
  function mkIface(interfaceName, apiName) {
    return WebInterface.create({
      interfaceName,
      apiName,
    });
  }
  function mkJunction(release, iface) {
    return Junction.create({
          id: [release.id, iface.id],
      sourceId: release.id,
      targetId: iface.id,
    });
  }
  function mkData(value, date, release, prevReleases, comparedReleases) {
    return BrowserMetricData.create({
      type: BrowserMetricDataType.FAILURE_TO_SHIP,
      browserName: release.browserName,
      value,
      date,
      release,
      prevReleases,
      comparedReleases,
    });
  }
  beforeEach(function() {
    Release = foam.lookup('org.chromium.apis.web.Release');
    WebInterface = foam.lookup('org.chromium.apis.web.WebInterface');
    Junction = foam.lookup('org.chromium.apis.web.ReleaseWebInterfaceJunction');
    FailureToShip = foam.lookup('org.chromium.apis.web.FailureToShip');
    BrowserMetricDataType =
      foam.lookup('org.chromium.apis.web.BrowserMetricDataType');
    BrowserMetricData =
      foam.lookup('org.chromium.apis.web.BrowserMetricData');
    container = global.createDAOContainer();
    runner = global.createLocalRunner({
      metricComputerTypes: [
        foam.lookup('org.chromium.apis.web.MetricComputerType').FAILURE_TO_SHIP,
      ],
    }, container);
    releases = container.releaseDAO;
    ifaces = container.webInterfaceDAO;
    junctions = container.releaseWebInterfaceJunctionDAO;
  });

  it('should handle simple case', function(done) {
    let alpha, beta, charlie;
    Promise.all([
      releases.put(Release.create({
        browserName: 'Alpha',
        browserVersion: '1',
        osName: 'Windows',
        osVersion: '10',
        releaseDate: date1,
      }, container)),
      releases.put(Release.create({
        browserName: 'Beta',
        browserVersion: '1',
        osName: 'Windows',
        osVersion: '10',
        releaseDate: date1,
      }, container)),
      releases.put(Release.create({
        browserName: 'Charlie',
        browserVersion: '1',
        osName: 'Windows',
        osVersion: '10',
        releaseDate: date1,
      }, container)),
    ]).then(function(releasesArray) {
      alpha = releasesArray[0];
      beta = releasesArray[1];
      charlie = releasesArray[2];

      const bOnly = mkIface('B', 'only');
      const cOnly1 = mkIface('C', 'only1');
      const cOnly2 = mkIface('C', 'only2');
      const abOnly = mkIface('AB', 'only');
      const acOnly = mkIface('AC', 'only');
      const bcOnly = mkIface('BC', 'only');
      const abcAll = mkIface('ABC', 'all');
      return Promise.all([
        ifaces.put(bOnly),
        ifaces.put(cOnly1),
        ifaces.put(cOnly2),
        ifaces.put(abOnly),
        ifaces.put(acOnly),
        ifaces.put(bcOnly),
        ifaces.put(abcAll),
        junctions.put(mkJunction(alpha, abOnly)),
        junctions.put(mkJunction(alpha, acOnly)),
        junctions.put(mkJunction(beta, bOnly)),
        junctions.put(mkJunction(beta, abOnly)),
        junctions.put(mkJunction(beta, bcOnly)),
        junctions.put(mkJunction(charlie, cOnly1)),
        junctions.put(mkJunction(charlie, cOnly2)),
        junctions.put(mkJunction(charlie, acOnly)),
        junctions.put(mkJunction(charlie, bcOnly)),
      ]);
    }).then(function() {
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
      done();
    });
  });

  it('should exclude API if this browser ever shipped it', function(done) {
    let alpha1, alpha2, beta1, beta2;
    // Use date1 and date3 to ensure that version 2 is more than a year after
    // version 1.
    Promise.all([
      releases.put(Release.create({
        browserName: 'Alpha',
        browserVersion: '1',
        osName: 'Windows',
        osVersion: '10',
        releaseDate: date1,
      }, container)),
      releases.put(Release.create({
        browserName: 'Alpha',
        browserVersion: '2',
        osName: 'Windows',
        osVersion: '10',
        releaseDate: date3,
      }, container)),
      releases.put(Release.create({
        browserName: 'Beta',
        browserVersion: '1',
        osName: 'Windows',
        osVersion: '10',
        releaseDate: date1,
      }, container)),
      releases.put(Release.create({
        browserName: 'Beta',
        browserVersion: '2',
        osName: 'Windows',
        osVersion: '10',
        releaseDate: date3,
      }, container)),
    ]).then(function(releasesArray) {
      alpha1 = releasesArray[0];
      alpha2 = releasesArray[1];
      beta1 = releasesArray[2];
      beta2 = releasesArray[3];

      const iface = mkIface('Removed', 'inB2');
      return Promise.all([
        ifaces.put(iface),
        junctions.put(mkJunction(alpha1, iface)),
        junctions.put(mkJunction(alpha2, iface)),
        junctions.put(mkJunction(beta1, iface)),
      ]);
    }).then(function() {
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
      done();
    });
  });

  it('should exclude APIs not shipped by even one compared release', function(done) {
    let alpha2, alpha2_1, beta2, beta2_1, charlie2, charlie2_1;
    // Use date2 and date2_1 to ensure two versions during grace period.
    Promise.all([
      releases.put(Release.create({
        browserName: 'Alpha',
        browserVersion: '2',
        osName: 'Windows',
        osVersion: '10',
        releaseDate: date2,
      }, container)),
      releases.put(Release.create({
        browserName: 'Alpha',
        browserVersion: '2.1',
        osName: 'Windows',
        osVersion: '10',
        releaseDate: date2_1,
      }, container)),
      releases.put(Release.create({
        browserName: 'Beta',
        browserVersion: '2',
        osName: 'Windows',
        osVersion: '10',
        releaseDate: date2,
      }, container)),
      releases.put(Release.create({
        browserName: 'Beta',
        browserVersion: '2.1',
        osName: 'Windows',
        osVersion: '10',
        releaseDate: date2_1,
      }, container)),
      releases.put(Release.create({
        browserName: 'Charlie',
        browserVersion: '2',
        osName: 'Windows',
        osVersion: '10',
        releaseDate: date2,
      }, container)),
      releases.put(Release.create({
        browserName: 'Charlie',
        browserVersion: '2.1',
        osName: 'Windows',
        osVersion: '10',
        releaseDate: date2_1,
      }, container)),
    ]).then(function(releasesArray) {
      alpha2 = releasesArray[0];
      alpha2_1 = releasesArray[1];
      beta2 = releasesArray[2];
      beta2_1 = releasesArray[3];
      charlie2 = releasesArray[4];
      charlie2_1 = releasesArray[5];

      const iface = mkIface('NotIn', 'charlie2');
      return Promise.all([
        ifaces.put(iface),
        junctions.put(mkJunction(beta2, iface)),
        junctions.put(mkJunction(beta2_1, iface)),
        junctions.put(mkJunction(charlie2_1, iface)),
      ]);
    }).then(function() {
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
      // The important bit: alpha2_1 does not register a failure to ship, even
      // though beta2_1 and charlie2_1 shipped an API that alpha2_1 does not.
      // This is because charlie2, which is within the grace period, did not
      // ship the API; failure to ship only registers when ALL releases of other
      // browsers within the grace period shipped the API.
      expect(sink.array[1].release.id).toBe('Alpha_2.1_Windows_10');
      expect(sink.array[1].value).toBe(0);
      done();
    });
  });
});
