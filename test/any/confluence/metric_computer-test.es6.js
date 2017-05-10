// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

describe('MetricComputer', function() {
  let metricComputer;
  let computeSpy;
  let release = function(browserName, browserVersion,
      osName, osVersion, releaseDate) {
        return org.chromium.apis.web.Release.create({
          browserName,
          browserVersion,
          osName,
          osVersion,
          releaseDate,
        });
      };
  let webInterface = function(interfaceName, apiName) {
    return org.chromium.apis.web.WebInterface.create({
      interfaceName,
      apiName,
    });
  };
  let releaseAPI = function(browserName, browserVersion,
      osName, osVersion, interfaceName, apiName) {
        return org.chromium.apis.web.ReleaseWebInterfaceJunction.create({
          sourceId: `${browserName}_${browserVersion}_${osName}_${osVersion}`,
          targetId: `${interfaceName}#${apiName}`,
        });
  };

  beforeEach(function() {
    let container = global.createReleaseApiContainer();

    computeSpy = jasmine.createSpy('computeSpy');

    foam.CLASS({
      package: 'org.chromium.apis.web',
      name: 'MetricComputerTester',
      extends: 'org.chromium.apis.web.MetricComputer',

      documentation: `A Test class for MetricComputer, with a spy
          in compute method`,

      methods: [
        {
          name: 'compute',
          documentation: `Compute aggressive removal value for each release in
              releases at the given date.`,
          code: computeSpy,
        },
      ],
    });

    let releaseDAO = container.releaseDAO;
    let webInterfaceDAO = container.webInterfaceDAO;
    let junctionDAO = container.releaseWebInterfaceJunctionDAO;

    junctionDAO.put(releaseAPI(
        'Chrome', '55', 'Windows', '10', 'Array', 'find'));
    junctionDAO.put(releaseAPI(
        'Chrome', '55', 'Windows', '10', 'Audio', 'stop'));
    junctionDAO.put(releaseAPI(
        'Edge', '14', 'Windows', '10', 'Array', 'find'));
    junctionDAO.put(releaseAPI(
        'Edge', '14', 'Windows', '10', 'Audio', 'play'));
    junctionDAO.put(releaseAPI(
        'Safari', '10', 'OSX', '601', 'ApplePay', 'about'));
    junctionDAO.put(releaseAPI(
        'Safari', '10', 'OSX', '601', 'Audio', 'play'));
    junctionDAO.put(releaseAPI(
        'Safari', '10', 'OSX', '601', 'Audio', 'stop'));
    junctionDAO.put(releaseAPI(
        'Safari', '10', 'OSX', '601', 'Array', 'find'));
    releaseDAO.put(release('Chrome', '55', 'Windows', '10',
        new Date('2015-01-10')));
    releaseDAO.put(release('Edge', '14', 'Windows', '10',
        new Date('2014-02-01')));
    releaseDAO.put(release('Safari', '10', 'OSX', '601',
        new Date('2015-04-01')));
    webInterfaceDAO.put(webInterface('Array', 'find'));
    webInterfaceDAO.put(webInterface('Audio', 'play'));
    webInterfaceDAO.put(webInterface('Audio', 'stop'));
    webInterfaceDAO.put(webInterface('ApplePay', 'about'));

    metricComputer = foam.lookup('org.chromium.apis.web.MetricComputerTester')
        .create(null, container);
  });
  describe('getOrderedListOfReleaseDates()', function() {
    it('gets correct dates from releaseWebInterfaceJunctionDAO', function() {
      metricComputer.getOrderedListOfReleaseDates().then((dateArr) => {
        expect(dateArr).toEqual([
          new Date('2014-02-01'),
          new Date('2015-01-10'),
          new Date('2015-04-01'),
        ])
      });
    });
  });
  describe('getLatestReleaseFromEachBrowserAtDate()', function() {
    it('gets correct releases before a given date.', function(done) {
      metricComputer.getLatestReleaseFromEachBrowserAtDate(
          new Date('2014-02-01')).then((releases) => {
            expect(releases.length).toBe(1);
            expect(releases[0].browserName).toBe('Edge');
            expect(releases[0].browserVersion).toBe('14');
            done();
          });
    });
    it('gets correct releases before a given date even if the' +
        ' releases list are empty.', function(done) {
          metricComputer.getLatestReleaseFromEachBrowserAtDate(
              new Date('2014-01-01')).then((releases) => {
                expect(releases.length).toBe(0);
                done();
              });
    });
    it("gets all browsers a date when all browsers has a release.",
        function(done) {
          metricComputer.getLatestReleaseFromEachBrowserAtDate(
              new Date('2015-05-01')).then((releases) => {
                expect(releases.length).toBe(3);
                expect(releases[0].browserName).toBe('Safari');
                expect(releases[0].browserVersion).toBe('10');
                expect(releases[1].browserName).toBe('Chrome');
                expect(releases[1].browserVersion).toBe('55');
                expect(releases[2].browserName).toBe('Edge');
                expect(releases[2].browserVersion).toBe('14');
                done();
              });
    });
  });
});
