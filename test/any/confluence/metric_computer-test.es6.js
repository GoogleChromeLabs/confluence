// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

describe('MetricComputer', function() {
  let metricComputer;
  let computeSpy;
  let browser = function(browserName, browserVersion,
      osName, osVersion, releaseDate) {
        return org.chromium.apis.web.Browser.create({
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
  let browserAPI = function(browserName, browserVersion,
      osName, osVersion, interfaceName, apiName) {
        return org.chromium.apis.web.BrowserWebInterfaceJunction.create({
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
          documentation: `Compute aggressive removal value for each browser in
              browsers at the given date.`,
          code: computeSpy,
        },
      ],
    });
    let browserDAO = foam.dao.EasyDAO.create({
      name: 'browserDAO',
      of: org.chromium.apis.web.Browser,
      daoType: 'MDAO',
    });
    let interfaceDAO = foam.dao.EasyDAO.create({
      name: 'interfaceDAO',
      of: org.chromium.apis.web.WebInterface,
      daoType: 'MDAO',
    });
    let browserApiDAO = foam.dao.EasyDAO.create({
      name: 'browserApiDao',
      of: org.chromium.apis.web.BrowserWebInterfaceJunction,
      daoType: 'MDAO',
    });
    browserApiDAO.put(browserAPI('Chrome', '55', 'Windows', '10',
        'Array', 'find'));
    browserApiDAO.put(browserAPI('Chrome', '55', 'Windows', '10',
        'Audio', 'stop'));
    browserApiDAO.put(browserAPI('Edge', '14', 'Windows', '10',
        'Array', 'find'));
    browserApiDAO.put(browserAPI('Edge', '14', 'Windows', '10',
        'Audio', 'play'));
    browserApiDAO.put(browserAPI('Safari', '10', 'OSX', '601',
        'ApplePay', 'about'));
    browserApiDAO.put(browserAPI('Safari', '10', 'OSX', '601',
        'Audio', 'play'));
    browserApiDAO.put(browserAPI('Safari', '10', 'OSX', '601',
        'Audio', 'stop'));
    browserApiDAO.put(browserAPI('Safari', '10', 'OSX', '601',
        'Array', 'find'));
    browserDAO.put(browser('Chrome', '55', 'Windows', '10',
        new Date('2015-01-10')));
    browserDAO.put(browser('Edge', '14', 'Windows', '10',
        new Date('2014-02-01')));
    browserDAO.put(browser('Safari', '10', 'OSX', '601',
        new Date('2015-04-01')));
    interfaceDAO.put(webInterface('Array', 'find'));
    interfaceDAO.put(webInterface('Audio', 'play'));
    interfaceDAO.put(webInterface('Audio', 'stop'));
    interfaceDAO.put(webInterface('ApplePay', 'about'));
    metricComputer = org.chromium.apis.web.MetricComputerTester.create({
      browserApiDAO,
      browserDAO,
      interfaceDAO,
    },
    // Provide a context that is aware to relationship DAOs.
    // TODO(markdittmer): providing an interface for binding
    // DAOs on Relationships.
    foam.__context__.createSubContext({
      browserDAO,
      webInterfaceDAO: interfaceDAO,
      browserWebInterfaceJunctionDAO: browserApiDAO,
    }));
  });
  describe('getOrderedListOfBrowserReleaseDates()', function() {
    it('gets correct dates from browserApiDAO', function() {
      metricComputer.getOrderedListOfBrowserReleaseDates().then((dateArr) => {
        expect(dateArr).toEqual([
          new Date('2014-02-01'),
          new Date('2015-01-10'),
          new Date('2015-04-01'),
        ])
      });
    });
  });
  describe('getLatestBrowserFromEachVendorAtDate()', function() {
    it('gets correct browsers before a given date.', function(done) {
      metricComputer.getLatestBrowserFromEachVendorAtDate(
          new Date('2014-02-01')).then((browsers) => {
            expect(browsers.length).toBe(1);
            expect(browsers[0].browserName).toBe('Edge');
            expect(browsers[0].browserVersion).toBe('14');
            done();
          });
    });
    it('gets correct browsers before a given date even if the' +
        ' browsers list are empty.', function(done) {
          metricComputer.getLatestBrowserFromEachVendorAtDate(
              new Date('2014-01-01')).then((browsers) => {
                expect(browsers.length).toBe(0);
                done();
              });
    });
    it("gets all vendor's browser a date when all browser has a release.",
        function(done) {
          metricComputer.getLatestBrowserFromEachVendorAtDate(
              new Date('2015-05-01')).then((browsers) => {
                expect(browsers.length).toBe(3);
                expect(browsers[0].browserName).toBe('Safari');
                expect(browsers[0].browserVersion).toBe('10');
                expect(browsers[1].browserName).toBe('Chrome');
                expect(browsers[1].browserVersion).toBe('55');
                expect(browsers[2].browserName).toBe('Edge');
                expect(browsers[2].browserVersion).toBe('14');
                done()
              });
    });
  });
})
