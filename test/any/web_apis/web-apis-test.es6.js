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

describe('WebAPIs', function() {
  let webAPIs = org.chromium.apis.web.WebAPIs.create();
  let M = foam.mlang.ExpressionsSingleton.create();
  let webCatalog = {
    'Window': [
      'Function',
      'property',
    ],
    'Function': [
      'arguments',
      'caller',
    ],
  };

  describe('importAPI()', function() {
    webAPIs.importAPI('Chrome', '56.0.2924.87', 'OSX', '10.12.2', webCatalog)
    .then((result) => {
      it('imports browser, interface pairs to BrowserAPIs', function(done) {
        expect(result).toBe(true);
        let interfaces = [
          {
            interfaceName: 'Window',
            apiName: 'Function',
          },
          {
            interfaceName: 'Window',
            apiName: 'property',
          },
          {
            interfaceName: 'Function',
            apiName: 'arguments',
          },
          {
            interfaceName: 'Function',
            apiName: 'caller',
          },
        ];
        let promises = interfaces.map((intface) => {
          return webAPIs.browserAPIs.where(M.AND(
              M.EQ(org.chromium.apis.web.BrowserAPI.BROWSER_NAME, 'Chrome'),
              M.EQ(org.chromium.apis.web.BrowserAPI.BROWSER_VERSION, '56.0.2924.87'),
              M.EQ(org.chromium.apis.web.BrowserAPI.OS_NAME, 'OSX'),
              M.EQ(org.chromium.apis.web.BrowserAPI.OS_VERSION, '10.12.2'),
              M.EQ(org.chromium.apis.web.BrowserAPI.INTERFACE_NAME,
                intface.interfaceName),
              M.EQ(org.chromium.apis.web.BrowserAPI.API_NAME, intface.apiName)
            )).select();
        });
        Promise.all(promises).then((results) => {
          results.forEach((defaultArrayDAO) => {
            expect(defaultArrayDAO.a.length).toBe(1);
          });
          done();
        });
      });
    });
  });

  describe('toMap_()', function() {
    let webAPIs = org.chromium.apis.web.WebAPIs.create();
    webAPIs.importAPI('Chrome', '55', 'Window', '10', {
      'Array': ['find'],
      'Audio': ['stop'],
    }).then(() => {
      webAPIs.importAPI('Edge', '14', 'Window', '10', {
        'Array': ['find'],
        'Audio': ['play'],
      });
    }).then(() => {
      webAPIs.importAPI('Safari', '10', 'OSX', '601', {
        'ApplePay': ['about'],
        'Audio': ['play', 'stop'],
        'Array': ['find'],
      });
    }).then(() => {
      describe('when all browser keys are selected', function() {
        it(`contains all browser keys in header array`, function(done) {
          webAPIs.browserAPIs.select().then((arrayDAO) => {
            let interfaceMap = webAPIs.toMap_(arrayDAO.a, [
              'Chrome_55_Window_10',
              'Edge_14_Window_10',
              'Safari_10_OSX_601',
            ]);
            expect(interfaceMap._header).toEqual([
              'Chrome_55_Window_10',
              'Edge_14_Window_10',
              'Safari_10_OSX_601',
            ]);
            done();
          });
        });
        it(`contains correct interface and API information`, function(done) {
          webAPIs.browserAPIs.select().then((arrayDAO) => {
            let interfaceMap = webAPIs.toMap_(arrayDAO.a, [
              'Chrome_55_Window_10',
              'Edge_14_Window_10',
              'Safari_10_OSX_601',
            ]);
            expect(interfaceMap.ApplePay.about).toEqual([
              false, false, true,
            ]);
            expect(interfaceMap.Array.find).toEqual([
              true, true, true,
            ]);
            expect(interfaceMap.Audio.play).toEqual([
              false, true, true,
            ]);
            expect(interfaceMap.Audio.stop).toEqual([
              true, false, true,
            ]);
            done();
          });
        });
      });
      describe('when part of browser keys are selected', function() {
        it(`contains selected browser keys in header`, function() {
          webAPIs.browserAPIs.select().then((arrayDAO) => {
            let interfaceMap = webAPIs.toMap_(arrayDAO.a, [
              'Chrome_55_Window_10',
              'Edge_14_Window_10',
            ]);
            expect(interfaceMap._header).toEqual([
              'Chrome_55_Window_10',
              'Edge_14_Window_10',
            ]);
          });
        });
        it(`contains correct interface and API information`, function(done) {
          webAPIs.browserAPIs.where(M.IN(org.chromium.apis.web.BrowserAPI.BROWSER_KEY,
            [
              'Chrome_55_Window_10',
              'Edge_14_Window_10',
            ])).select()
          .then((arrayDAO) => {
            let interfaceMap = webAPIs.toMap_(arrayDAO.a, [
              'Chrome_55_Window_10',
              'Edge_14_Window_10',
            ]);
            expect(interfaceMap.Array.find).toEqual([
              true, true,
            ]);
            expect(interfaceMap.Audio.play).toEqual([
              false, true,
            ]);
            expect(interfaceMap.Audio.stop).toEqual([
              true, false,
            ]);
            done();
          });
        });
      });
    });
  });

  describe('toMap()', function() {
    let webAPIs = org.chromium.apis.web.WebAPIs.create();
    webAPIs.importAPI('Chrome', '55', 'Window', '10', {
      'Array': ['find'],
      'Audio': ['stop'],
    }).then(() => {
      webAPIs.importAPI('Edge', '14', 'Window', '10', {
        'Array': ['find'],
        'Audio': ['play'],
      });
    }).then(() => {
      webAPIs.importAPI('Safari', '10', 'OSX', '601', {
        'ApplePay': ['about'],
        'Audio': ['play', 'stop'],
        'Array': ['find'],
      });
    }).then(() => {
      describe('When all browsers are selected', function() {
        it(`contains all browser keys in header array`, function(done) {
          webAPIs.toMap([
            'Chrome_55_Window_10',
            'Edge_14_Window_10',
            'Safari_10_OSX_601',
          ]).then((interfaceMap) => {
            expect(interfaceMap._header).toEqual([
              'Chrome_55_Window_10',
              'Edge_14_Window_10',
              'Safari_10_OSX_601',
            ]);
            done();
          });
        });
        it(`contains correct interface and API information`, function(done) {
          webAPIs.toMap([
            'Chrome_55_Window_10',
            'Edge_14_Window_10',
            'Safari_10_OSX_601',
          ]).then((interfaceMap) => {
            expect(interfaceMap.ApplePay.about).toEqual([
              false, false, true,
            ]);
            expect(interfaceMap.Array.find).toEqual([
              true, true, true,
            ]);
            expect(interfaceMap.Audio.play).toEqual([
              false, true, true,
            ]);
            expect(interfaceMap.Audio.stop).toEqual([
              true, false, true,
            ]);
            done();
          });
        });
      });
      describe('when part of browser keys are selected', function() {
        it(`contains selected browser keys in header`, function() {
          webAPIs.toMap([
            'Chrome_55_Window_10',
            'Edge_14_Window_10',
          ]).then((interfaceMap) => {
            expect(interfaceMap._header).toEqual([
              'Chrome_55_Window_10',
              'Edge_14_Window_10',
            ]);
          });
        });
        it(`contains correct interface and API information`, function(done) {
          webAPIs.toMap([
            'Chrome_55_Window_10',
            'Edge_14_Window_10',
          ]).then((interfaceMap) => {
            expect(interfaceMap.Array.find).toEqual([
              true, true,
            ]);
            expect(interfaceMap.Audio.play).toEqual([
              false, true,
            ]);
            expect(interfaceMap.Audio.stop).toEqual([
              true, false,
            ]);
            done();
          });
        });
      });
    });
  });

  describe('toCSV()', function() {
    let webAPIs = org.chromium.apis.web.WebAPIs.create();
    webAPIs.importAPI('Chrome', '55', 'Window', '10', {
      'Array': ['find'],
      'Audio': ['stop'],
    }).then(() => {
      webAPIs.importAPI('Edge', '14', 'Window', '10', {
        'Array': ['find'],
        'Audio': ['play'],
      });
    }).then(() => {
      webAPIs.importAPI('Safari', '10', 'OSX', '601', {
        'ApplePay': ['about'],
        'Audio': ['play', 'stop'],
        'Array': ['find'],
      });
    }).then(() => {
      it(`produces correct csv string when all browsers are selected`,
      function(done) {
        webAPIs.toCSV([
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
          done();
        });
      });
      it(`produces correct csv string when part of browsers are selected`,
      function(done) {
        webAPIs.toCSV([
          'Chrome_55_Window_10',
          'Edge_14_Window_10',
        ]).then((csvStr) => {
          expect(csvStr).toEqual(
            'Interface,API,Chrome_55_Window_10,Edge_14_Window_10\n' +
            'Array,find,true,true\n' + 'Audio,stop,true,false\n' +
            'Audio,play,false,true\n'
          );
          done();
        });
      });
    });
  });
});
