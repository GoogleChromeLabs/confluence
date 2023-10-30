// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

describe('MetricComputerRunner', function() {
  let runner;
  const release = function(browserName, browserVersion,
      osName, osVersion, releaseDate) {
    return org.chromium.apis.web.Release.create({
      browserName,
      browserVersion,
      osName,
      osVersion,
      releaseDate,
    });
  };
  const webInterface = function(interfaceName, apiName) {
    return org.chromium.apis.web.WebInterface.create({
      interfaceName,
      apiName,
    });
  };
  const releaseAPI = function(browserName, browserVersion,
      osName, osVersion, interfaceName, apiName) {
    return org.chromium.apis.web.ReleaseWebInterfaceJunction.create({
      sourceId: `${browserName}_${browserVersion}_${osName}_${osVersion}`,
      targetId: `${interfaceName}#${apiName}`,
    });
  };

  beforeEach(function() {
    const container = global.createDAOContainer();
    const E = foam.mlang.ExpressionsSingleton.create();

    foam.CLASS({
      package: 'org.chromium.apis.web',
      name: 'FakeMetricComputerService',
      extends: 'org.chromium.apis.web.MetricComputerService',

      properties: [
        {
          // Required.
          name: 'releasePredicate',
          factory: function() {
            return E.TRUE();
          },
        },
      ],

      methods: [
        function compute() {
          return Promise.resolve();
        },
      ],
    });

    foam.CLASS({
      package: 'org.chromium.apis.web',
      name: 'MetricComputerRunnerTester',
      extends: 'org.chromium.apis.web.MetricComputerRunner',

      documentation: `A Test class for MetricComputerRunner, with a fake
          service.`,

      requires: ['org.chromium.apis.web.FakeMetricComputerService'],

      properties: [
        {
          name: 'metricComputerService',
          factory: function() {
            return this.FakeMetricComputerService.create();
          },
        },
      ],
    });

    const releaseDAO = container.releaseDAO;
    const webInterfaceDAO = container.webInterfaceDAO;
    const junctionDAO = container.releaseWebInterfaceJunctionDAO;

    junctionDAO.put(releaseAPI(
        'Chrome', '55', 'Windows', '10', 'Array', 'find'));
    junctionDAO.put(releaseAPI(
        'Chrome', '55', 'Windows', '10', 'Audio', 'stop'));
    junctionDAO.put(releaseAPI(
        'Firefox', '34', 'Windows', '10', 'Array', 'find'));
    junctionDAO.put(releaseAPI(
        'Firefox', '34', 'Windows', '10', 'Audio', 'play'));
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
    releaseDAO.put(release('Firefox', '34', 'Windows', '10',
        new Date('2014-12-01')));
    releaseDAO.put(release('Safari', '10', 'OSX', '601',
        new Date('2015-04-01')));
    webInterfaceDAO.put(webInterface('Array', 'find'));
    webInterfaceDAO.put(webInterface('Audio', 'play'));
    webInterfaceDAO.put(webInterface('Audio', 'stop'));
    webInterfaceDAO.put(webInterface('ApplePay', 'about'));

    runner = foam.lookup('org.chromium.apis.web.MetricComputerRunnerTester')
        .create(null, container);
  });
  describe('getOrderedListOfReleaseDates()', function() {
    it('gets correct dates from releaseWebInterfaceJunctionDAO', function(done) {
      runner.getOrderedListOfReleaseDates().then((dateArr) => {
        expect(dateArr).toEqual([
          new Date('2014-12-01'),
          new Date('2015-01-10'),
          new Date('2015-04-01'),
        ]);
      }).then(done, done.fail);
    });
  });
  describe('getLatestReleaseFromEachBrowserAtDate()', function() {
    it('gets correct releases before a given date.', function(done) {
      runner.getLatestReleaseFromEachBrowserAtDate(
          new Date('2014-12-01')).then((releases) => {
        expect(releases.length).toBe(1);
        expect(releases[0].browserName).toBe('Firefox');
        expect(releases[0].browserVersion).toBe('34');
        done();
      });
    });
    it('gets correct releases before a given date even if the' +
        ' releases list are empty.', function(done) {
      runner.getLatestReleaseFromEachBrowserAtDate(
          new Date('2014-01-01')).then((releases) => {
        expect(releases.length).toBe(0);
        done();
      });
    });
    it('gets all browsers a date when all browsers has a release.',
        function(done) {
          runner.getLatestReleaseFromEachBrowserAtDate(
              new Date('2015-05-01')).then((releases) => {
            expect(releases.length).toBe(3);
            expect(releases[0].browserName).toBe('Safari');
            expect(releases[0].browserVersion).toBe('10');
            expect(releases[1].browserName).toBe('Chrome');
            expect(releases[1].browserVersion).toBe('55');
            expect(releases[2].browserName).toBe('Firefox');
            expect(releases[2].browserVersion).toBe('34');
            done();
          });
        });
  });
});
