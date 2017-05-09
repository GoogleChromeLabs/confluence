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
    let releaseDAO = foam.dao.EasyDAO.create({
      name: 'releaseDAO',
      of: org.chromium.apis.web.Release,
      daoType: 'MDAO',
    });
    let interfaceDAO = foam.dao.EasyDAO.create({
      name: 'interfaceDAO',
      of: org.chromium.apis.web.WebInterface,
      daoType: 'MDAO',
    });
    let releaseApiDAO = foam.dao.EasyDAO.create({
      name: 'releaseApiDao',
      of: org.chromium.apis.web.ReleaseWebInterfaceJunction,
      daoType: 'MDAO',
    });
    releaseApiDAO.put(releaseAPI('Chrome', '55', 'Windows', '10',
        'Array', 'find'));
    releaseApiDAO.put(releaseAPI('Chrome', '55', 'Windows', '10',
        'Audio', 'stop'));
    releaseApiDAO.put(releaseAPI('Edge', '14', 'Windows', '10',
        'Array', 'find'));
    releaseApiDAO.put(releaseAPI('Edge', '14', 'Windows', '10',
        'Audio', 'play'));
    releaseApiDAO.put(releaseAPI('Safari', '10', 'OSX', '601',
        'ApplePay', 'about'));
    releaseApiDAO.put(releaseAPI('Safari', '10', 'OSX', '601',
        'Audio', 'play'));
    releaseApiDAO.put(releaseAPI('Safari', '10', 'OSX', '601',
        'Audio', 'stop'));
    releaseApiDAO.put(releaseAPI('Safari', '10', 'OSX', '601',
        'Array', 'find'));
    releaseDAO.put(release('Chrome', '55', 'Windows', '10',
        new Date('2015-01-10')));
    releaseDAO.put(release('Edge', '14', 'Windows', '10',
        new Date('2014-02-01')));
    releaseDAO.put(release('Safari', '10', 'OSX', '601',
        new Date('2015-04-01')));
    interfaceDAO.put(webInterface('Array', 'find'));
    interfaceDAO.put(webInterface('Audio', 'play'));
    interfaceDAO.put(webInterface('Audio', 'stop'));
    interfaceDAO.put(webInterface('ApplePay', 'about'));
    metricComputer = org.chromium.apis.web.MetricComputerTester.create({
      releaseApiDAO,
      releaseDAO,
      interfaceDAO,
    },
    // Provide a context that is aware to relationship DAOs.
    // TODO(markdittmer): providing an interface for binding
    // DAOs on Relationships.
    foam.__context__.createSubContext({
      releaseDAO,
      webInterfaceDAO: interfaceDAO,
      releaseWebInterfaceJunctionDAO: releaseApiDAO,
    }));
  });
  describe('getOrderedListOfReleaseReleaseDates()', function() {
    it('gets correct dates from releaseApiDAO', function() {
      metricComputer.getOrderedListOfReleaseReleaseDates().then((dateArr) => {
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
                done()
              });
    });
  });
})
