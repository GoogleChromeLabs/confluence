/**
 * @license
 * Copyright 2017 Google Inc. All Rights Reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the 'License');
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an 'AS IS' BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
'use strict';

describe('ApiMatrix', function() {
  let apiMatrix;
  beforeEach(function() {
    let browserApiDao = foam.dao.EasyDAO.create({
      name: 'browserApiDao',
      of: org.chromium.apis.web.BrowserAPI,
      daoType: 'MDAO',
    });
    browserApiDao.put(org.chromium.apis.web.BrowserAPI.create({
      browserName: 'Chrome',
      browserVersion: '55',
      osName: 'Window',
      osVersion: '10',
      interfaceName: 'Array',
      apiName: 'find',
    }));
    browserApiDao.put(org.chromium.apis.web.BrowserAPI.create({
      browserName: 'Chrome',
      browserVersion: '55',
      osName: 'Window',
      osVersion: '10',
      interfaceName: 'Audio',
      apiName: 'stop',
    }));
    browserApiDao.put(org.chromium.apis.web.BrowserAPI.create({
      browserName: 'Edge',
      browserVersion: '14',
      osName: 'Window',
      osVersion: '10',
      interfaceName: 'Array',
      apiName: 'find',
    }));
    browserApiDao.put(org.chromium.apis.web.BrowserAPI.create({
      browserName: 'Edge',
      browserVersion: '14',
      osName: 'Window',
      osVersion: '10',
      interfaceName: 'Audio',
      apiName: 'play',
    }));
    browserApiDao.put(org.chromium.apis.web.BrowserAPI.create({
      browserName: 'Safari',
      browserVersion: '10',
      osName: 'OSX',
      osVersion: '601',
      interfaceName: 'ApplePay',
      apiName: 'about',
    }));
    browserApiDao.put(org.chromium.apis.web.BrowserAPI.create({
      browserName: 'Safari',
      browserVersion: '10',
      osName: 'OSX',
      osVersion: '601',
      interfaceName: 'Audio',
      apiName: 'play',
    }));
    browserApiDao.put(org.chromium.apis.web.BrowserAPI.create({
      browserName: 'Safari',
      browserVersion: '10',
      osName: 'OSX',
      osVersion: '601',
      interfaceName: 'Audio',
      apiName: 'stop',
    }));
    browserApiDao.put(org.chromium.apis.web.BrowserAPI.create({
      browserName: 'Safari',
      browserVersion: '10',
      osName: 'OSX',
      osVersion: '601',
      interfaceName: 'Array',
      apiName: 'find',
    }));
    apiMatrix = org.chromium.apis.web.ApiMatrix.create({
      browserAPIs: browserApiDao,
    });
  });

  describe('toMatrix()', function() {
    it(`contains correct interface and API information, when all browsers
      are selected`, function() {
        apiMatrix.toMatrix([
          'Chrome_55_Window_10',
          'Edge_14_Window_10',
          'Safari_10_OSX_601',
        ]).then((interfaceMatrix) => {
          expect(interfaceMatrix.ApplePay.about).toEqual({
            Safari_10_OSX_601: true,
          });
          expect(interfaceMatrix.ApplePay.about.Chrome_55_Window_10)
            .toBeUndefined();
          expect(interfaceMatrix.ApplePay.about.Edge_14_Window_10)
            .toBeUndefined();
          expect(interfaceMatrix.Array.find).toEqual({
            Chrome_55_Window_10: true,
            Edge_14_Window_10: true,
            Safari_10_OSX_601: true,
          });
          expect(interfaceMatrix.Audio.play).toEqual({
            Edge_14_Window_10: true,
            Safari_10_OSX_601: true,
          });
          expect(interfaceMatrix.Audio.stop).toEqual({
            Chrome_55_Window_10: true,
            Safari_10_OSX_601: true,
          });
        });
      });
    it(`contains correct interface and API information, when part of
      keys are selected.`, function() {
        apiMatrix.toMatrix([
          'Chrome_55_Window_10',
          'Edge_14_Window_10',
        ]).then((interfaceMatrix) => {
          expect(interfaceMatrix.Array.find).toEqual({
            Chrome_55_Window_10: true,
            Edge_14_Window_10: true,
          });
          expect(interfaceMatrix.Audio.play).toEqual({
            Edge_14_Window_10: true,
          });
          expect(interfaceMatrix.Audio.stop).toEqual({
            Chrome_55_Window_10: true,
          });
        });
      });
    it('returns empty object when given array of browser keys is empty.',
      function() {
        apiMatrix.toMatrix([]).then((interfaceMatrix) => {
          expect(interfaceMatrix).toEqual({});
        });
    });
    it('produces correct result when unknown browser key is given.',
      function() {
        apiMatrix.toMatrix([
          'Chrome_55_Window_10',
          'IE_10_Window_8',
        ]).then((interfaceMatrix) => {
          expect(interfaceMatrix).toEqual({
            Array: {
              find: {
                Chrome_55_Window_10: true,
              },
            },
            Audio: {
              stop: {
                Chrome_55_Window_10: true,
              },
            },
          });
        });
    });
  });

  describe('toCSV()', function() {
    it(`produces correct csv string when all browsers are selected`,
    function() {
      apiMatrix.toCSV([
        'Chrome_55_Window_10',
        'Edge_14_Window_10',
        'Safari_10_OSX_601',
      ]).then((csvStr) => {
        expect(csvStr.split('\n').sort()).toEqual(
          ('Interface,API,Chrome_55_Window_10,Edge_14_Window_10,' +
          'Safari_10_OSX_601\n' + 'Array,find,true,true,true\n' +
          'Audio,play,false,true,true\n' + 'Audio,stop,true,false,true\n' +
          'ApplePay,about,false,false,true\n').split('\n').sort()
        );
      });
    });
    it(`produces correct csv string when part of browsers are selected`,
    function() {
      apiMatrix.toCSV([
        'Chrome_55_Window_10',
        'Edge_14_Window_10',
      ]).then((csvStr) => {
        expect(csvStr.split('\n').sort()).toEqual(
          ('Interface,API,Chrome_55_Window_10,Edge_14_Window_10\n' +
          'Array,find,true,true\n' + 'Audio,stop,true,false\n' +
          'Audio,play,false,true\n').split('\n').sort()
        );
      });
    });
    it('returns empty csv when given array of browser keys is empty.',
      function() {
        apiMatrix.toCSV([]).then((csvStr) => {
          expect(csvStr).toEqual('Interface,API\n');
        });
    });
    it('lists  all false for a unknown browser key.',
      function() {
        apiMatrix.toCSV([
          'Chrome_55_Window_10',
          'IE_10_Window_8',
        ]).then((csvStr) => {
          expect(csvStr.split('\n').sort()).toEqual(
            ('Interface,API,Chrome_55_Window_10,IE_10_Window_8\n' +
            'Array,find,true,false\n' +
            'Audio,stop,true,false\n').split('\n').sort());
        });
    });
    it('has a table header with the same order of given browser keys',
      function() {
        apiMatrix.toCSV([
          'Edge_14_Window_10',
          'Safari_10_OSX_601',
          'Chrome_55_Window_10',
        ]).then((csvStr) => {
          expect(csvStr.split('\n')[0]).toEqual(
            'Interface,API,Edge_14_Window_10,' +
            'Safari_10_OSX_601,Chrome_55_Window_10'
          );
        });
      });
  });
});
