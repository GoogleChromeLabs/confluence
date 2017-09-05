// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

describe('ApiMatrix', function() {
  let Release;
  let WebInterface;
  let Junction;
  let ApiMatrix;
  let apiMatrix;

  let release = function(browserName, browserVersion,
    osName, osVersion) {
      return Release.create({
        browserName,
        browserVersion,
        osName,
        osVersion,
      });
    };
  let webInterface = function(interfaceName, apiName) {
    return WebInterface.create({interfaceName, apiName});
  };
  let releaseAPI = function(browserName, browserVersion,
      osName, osVersion, interfaceName, apiName) {
    return Junction.create({
      sourceId: `${browserName}_${browserVersion}_${osName}_${osVersion}`,
      targetId: `${interfaceName}#${apiName}`,
    });
  };

  beforeEach(function() {
    Release = foam.lookup('org.chromium.apis.web.Release');
    WebInterface = foam.lookup('org.chromium.apis.web.WebInterface');
    Junction = foam.lookup('org.chromium.apis.web.ReleaseWebInterfaceJunction');
    ApiMatrix = foam.lookup('org.chromium.apis.web.ApiMatrix');

    let container = global.createDAOContainer();
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
    releaseDAO.put(release('Chrome', '55', 'Windows', '10'));
    releaseDAO.put(release('Edge', '14', 'Windows', '10'));
    releaseDAO.put(release('Safari', '10', 'OSX', '601'));
    webInterfaceDAO.put(webInterface('Array', 'find'));
    webInterfaceDAO.put(webInterface('Audio', 'play'));
    webInterfaceDAO.put(webInterface('Audio', 'stop'));
    webInterfaceDAO.put(webInterface('ApplePay', 'about'));

    apiMatrix = ApiMatrix.create(null, container);
  });

  describe('toMatrix()', function() {
    it(`contains correct interface and API information, when all releases
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
    it('returns empty object when given array of release keys is empty.',
      function(done) {
        apiMatrix.toMatrix([]).then((interfaceMatrix) => {
          expect(interfaceMatrix).toEqual({});
          done();
        });
    });
    it('rejects promise if unknown release key are given.',
      function(done) {
        apiMatrix.toMatrix([
          'Chrome_55_Windows_10',
          'IE_10_Windows_8',
        ]).then(() => {
          fail('toMatrix promise resolved on unknwon release key.');
        }, (reason) => {
          expect(reason).toEqual(new Error('IE_10_Windows_8 does not exist.'));
          done();
        });
    });
    it(`filters APIs by options.releaseOptions`, function(done) {
      apiMatrix.toMatrix([
        'Chrome_55_Windows_10',
        'Edge_14_Windows_10',
        'Safari_10_OSX_601',
      ], {
        releaseOptions: {
          'Chrome_55_Windows_10': true,
          'Edge_14_Windows_10': false,
        },
      }).then((interfaceMatrix) => {
        expect(interfaceMatrix.ApplePay).toBeUndefined();
        expect(interfaceMatrix.Array).toBeUndefined();
        expect(interfaceMatrix.Audio.play).toBeUndefined();
        expect(interfaceMatrix.Audio.stop).toEqual({
          Chrome_55_Windows_10: true,
          Safari_10_OSX_601: true,
        });
      }).then(function() {
        return apiMatrix.toMatrix([
          'Chrome_55_Windows_10',
          'Edge_14_Windows_10',
          'Safari_10_OSX_601',
        ], {
          releaseOptions: {
            'Chrome_55_Windows_10': false,
          },
        });
      }).then((interfaceMatrix) => {
        expect(interfaceMatrix.ApplePay.about).toEqual({
          Safari_10_OSX_601: true,
        });
        expect(interfaceMatrix.Audio.play).toEqual({
          Edge_14_Windows_10: true,
          Safari_10_OSX_601: true,
        });
        expect(interfaceMatrix.Array).toBeUndefined();
        expect(interfaceMatrix.Audio.stop).toBeUndefined();
      }).then(done, done.fail);
    });
    it(`filters APIs by options.numAvailable`, function(done) {
      apiMatrix.toMatrix([
        'Chrome_55_Windows_10',
        'Edge_14_Windows_10',
        'Safari_10_OSX_601',
      ], {
        numAvailable: 1,
      }).then((interfaceMatrix) => {
        expect(interfaceMatrix).toEqual({
          ApplePay: {
            about: {
              Safari_10_OSX_601: true,
            },
          },
        });
      }).then(function() {
        return apiMatrix.toMatrix([
          'Chrome_55_Windows_10',
          'Edge_14_Windows_10',
          'Safari_10_OSX_601',
        ], {
          numAvailable: [2, 3],
        });
      }).then((interfaceMatrix) => {
        expect(interfaceMatrix.Array.find).toBeDefined();
        expect(interfaceMatrix.Audio.play).toBeDefined();
        expect(interfaceMatrix.Audio.stop).toBeDefined();
        expect(interfaceMatrix.ApplePay).toBeUndefined();
      }).then(done, done.fail);
    });
  });

  describe('toCSV()', function() {
    it(`produces correct csv string when all releases are selected`,
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
    it(`produces correct csv string when part of releases are selected`,
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
    it('returns empty csv when given array of release keys is empty.',
      function(done) {
        apiMatrix.toCSV([]).then((csvStr) => {
          expect(csvStr).toEqual('Interface,API\n');
          done();
        });
    });
    it('rejects promise if unknown release key are given.',
      function(done) {
        apiMatrix.toCSV([
          'Chrome_55_Windows_10',
          'IE_10_Windows_8',
        ]).then(() => {
          fail('toCSV promise resolved on unknwon release key.');
        }, (reason) => {
          expect(reason).toEqual(new Error('IE_10_Windows_8 does not exist.'));
          done();
        });
    });
    it('has a table header with the same order of given release keys',
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
