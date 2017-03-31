// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

describe('ApiMatrix', function() {
  let apiMatrix;
  let browser = function(browserName, browserVersion,
    osName, osVersion) {
      return org.chromium.apis.web.Browser.create({
        browserName,
        browserVersion,
        osName,
        osVersion,
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
    browserDAO.put(browser('Chrome', '55', 'Windows', '10'));
    browserDAO.put(browser('Edge', '14', 'Windows', '10'));
    browserDAO.put(browser('Safari', '10', 'OSX', '601'));
    interfaceDAO.put(webInterface('Array', 'find'));
    interfaceDAO.put(webInterface('Audio', 'play'));
    interfaceDAO.put(webInterface('Audio', 'stop'));
    interfaceDAO.put(webInterface('ApplePay', 'about'));
    apiMatrix = org.chromium.apis.web.ApiMatrix.create({
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

  describe('toMatrix()', function() {
    it(`contains correct interface and API information, when all browsers
      are selected`, function(done) {
        apiMatrix.toMatrix([
          'Chrome_55_Windows_10',
          'Edge_14_Windows_10',
          'Safari_10_OSX_601',
        ]).then((interfaceMatrix) => {
          expect(interfaceMatrix.ApplePay.about).toEqual({
            Safari_10_OSX_601: true,
          });
          expect(interfaceMatrix.ApplePay.about.Chrome_55_Windows_10)
            .toBeUndefined();
          expect(interfaceMatrix.ApplePay.about.Edge_14_Windows_10)
            .toBeUndefined();
          expect(interfaceMatrix.Array.find).toEqual({
            Chrome_55_Windows_10: true,
            Edge_14_Windows_10: true,
            Safari_10_OSX_601: true,
          });
          expect(interfaceMatrix.Audio.play).toEqual({
            Edge_14_Windows_10: true,
            Safari_10_OSX_601: true,
          });
          expect(interfaceMatrix.Audio.stop).toEqual({
            Chrome_55_Windows_10: true,
            Safari_10_OSX_601: true,
          });
          done();
        });
      });
    it(`contains correct interface and API information, when part of
      keys are selected.`, function(done) {
        apiMatrix.toMatrix([
          'Chrome_55_Windows_10',
          'Edge_14_Windows_10',
        ]).then((interfaceMatrix) => {
          expect(interfaceMatrix.Array.find).toEqual({
            Chrome_55_Windows_10: true,
            Edge_14_Windows_10: true,
          });
          expect(interfaceMatrix.Audio.play).toEqual({
            Edge_14_Windows_10: true,
          });
          expect(interfaceMatrix.Audio.stop).toEqual({
            Chrome_55_Windows_10: true,
          });
          done();
        });
      });
    it('returns empty object when given array of browser keys is empty.',
      function(done) {
        apiMatrix.toMatrix([]).then((interfaceMatrix) => {
          expect(interfaceMatrix).toEqual({});
          done();
        });
    });
    it('rejects promise if unknown browser key are given.',
      function(done) {
        apiMatrix.toMatrix([
          'Chrome_55_Windows_10',
          'IE_10_Windows_8',
        ]).then(() => {
          fail('toMatrix promise resolved on unknwon browser key.');
        }, (reason) => {
          expect(reason).toEqual(new Error('IE_10_Windows_8 does not exist.'));
          done();
        });
    });
  });

  describe('toCSV()', function() {
    it(`produces correct csv string when all browsers are selected`,
    function(done) {
      apiMatrix.toCSV([
        'Chrome_55_Windows_10',
        'Edge_14_Windows_10',
        'Safari_10_OSX_601',
      ]).then((csvStr) => {
        expect(csvStr).toEqual(
          'Interface,API,Chrome_55_Windows_10,Edge_14_Windows_10,' +
          'Safari_10_OSX_601\n' + 'ApplePay,about,false,false,true\n' +
          'Array,find,true,true,true\n' + 'Audio,play,false,true,true\n' +
          'Audio,stop,true,false,true\n');
        done();
      });
    });
    it(`produces correct csv string when part of browsers are selected`,
    function(done) {
      apiMatrix.toCSV([
        'Chrome_55_Windows_10',
        'Edge_14_Windows_10',
      ]).then((csvStr) => {
        expect(csvStr).toEqual(
          'Interface,API,Chrome_55_Windows_10,Edge_14_Windows_10\n' +
          'Array,find,true,true\n' + 'Audio,play,false,true\n' +
          'Audio,stop,true,false\n');
        done();
      });
    });
    it('returns empty csv when given array of browser keys is empty.',
      function(done) {
        apiMatrix.toCSV([]).then((csvStr) => {
          expect(csvStr).toEqual('Interface,API\n');
          done();
        });
    });
    it('rejects promise if unknown browser key are given.',
      function(done) {
        apiMatrix.toCSV([
          'Chrome_55_Windows_10',
          'IE_10_Windows_8',
        ]).then(() => {
          fail('toCSV promise resolved on unknwon browser key.');
        }, (reason) => {
          expect(reason).toEqual(new Error('IE_10_Windows_8 does not exist.'));
          done();
        });
    });
    it('has a table header with the same order of given browser keys',
      function(done) {
        apiMatrix.toCSV([
          'Edge_14_Windows_10',
          'Safari_10_OSX_601',
          'Chrome_55_Windows_10',
        ]).then((csvStr) => {
          expect(csvStr.split('\n')[0]).toEqual(
            'Interface,API,Edge_14_Windows_10,' +
            'Safari_10_OSX_601,Chrome_55_Windows_10'
          );
          done();
        });
      });
  });
});
