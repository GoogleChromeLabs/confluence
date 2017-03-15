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
  let webAPIs = com.web.api.WebAPIs.create();
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
  webAPIs.importAPI('Chrome', '56.0.2924.87', 'OSX', '10.12.2', webCatalog);

  describe('importAPI()', function() {
    it('correctly imports browser object', function(done) {
      webAPIs.browsers.where(M.AND(
        M.EQ(com.web.api.Browser.BROWSER_NAME, 'Chrome'),
        M.EQ(com.web.api.Browser.BROWSER_VERSION, '56.0.2924.87'),
        M.EQ(com.web.api.Browser.OS, 'OSX'),
        M.EQ(com.web.api.Browser.OS_VERSION, '10.12.2'))).select()
      .then((arrayDAO) => {
        expect(arrayDAO.a.length).toBe(1);
        done();
      });
    });
    it('correctly imports WebInterface objects', function(done) {
      let promises = [];
      promises.push(webAPIs.interfaces.where(M.AND(
          M.EQ(com.web.api.WebInterface.INTERFACE_NAME, 'Window'),
          M.EQ(com.web.api.WebInterface.APINAME, 'Function')))
        .select());
      promises.push(webAPIs.interfaces.where(M.AND(
          M.EQ(com.web.api.WebInterface.INTERFACE_NAME, 'Window'),
          M.EQ(com.web.api.WebInterface.APINAME, 'property')))
        .select());
      promises.push(webAPIs.interfaces.where(M.AND(
          M.EQ(com.web.api.WebInterface.INTERFACE_NAME, 'Function'),
          M.EQ(com.web.api.WebInterface.APINAME, 'arguments')))
        .select());
      promises.push(webAPIs.interfaces.where(M.AND(
          M.EQ(com.web.api.WebInterface.INTERFACE_NAME, 'Function'),
          M.EQ(com.web.api.WebInterface.APINAME, 'caller')))
        .select());
      Promise.all(promises).then((results) => {
        results.forEach((defaultArrayDAO) => {
          expect(defaultArrayDAO.a.length).toBe(1);
        });
        done();
      });
    });
    it('imports browser Id and Interface Id to BrowserAPIs', function(done) {
      let browserId = com.web.api.Browser.create({
        browserName: 'Chrome',
        browserVersion: '56.0.2924.87',
        os: 'OSX',
        osVersion: '10.12.2',
      }).id;
      let interfaceIds = [
        com.web.api.WebInterface.create({
          interfaceName: 'Window',
          APIName: 'Function',
        }).id,
        com.web.api.WebInterface.create({
          interfaceName: 'Window',
          APIName: 'property',
        }).id,
        com.web.api.WebInterface.create({
          interfaceName: 'Function',
          APIName: 'arguments',
        }).id,
        com.web.api.WebInterface.create({
          interfaceName: 'Function',
          APIName: 'caller',
        }).id,
      ];
      let promises = interfaceIds.map((interfaceId) => {
        return webAPIs.browserAPIs.where(M.AND(
            M.EQ(com.web.api.BrowserAPI.BROWSER_ID, browserId),
            M.EQ(com.web.api.BrowserAPI.INTERFACE_ID, interfaceId)
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

  webAPIs.importAPI('Chrome', '55.123.45', 'OSX', '10.12.2', webCatalog);
  webAPIs.importAPI('Chrome', '55.321', 'Windows', '10', webCatalog);
  webAPIs.importAPI('Chrome', '55.321', 'Windows', '8', webCatalog);
  webAPIs.importAPI('Chrome', '55.321', 'Windows', '7', webCatalog);
  webAPIs.importAPI('Firefox', '50', 'Windows', '10', webCatalog);
  describe('getBrowserKeys()', function() {
    it('returns all existing keys if no arguments are given', function(done) {
      webAPIs.getBrowserKeys().then((keys) => {
        expect(keys.sort()).toEqual([
          'Chrome_56.0.2924.87_OSX_10.12.2',
          'Chrome_55.123.45_OSX_10.12.2',
          'Chrome_55.321_Windows_10',
          'Chrome_55.321_Windows_8',
          'Chrome_55.321_Windows_7',
          'Firefox_50_Windows_10',
          ].sort());
        done();
      });
    });
    it('returns keys for a specific browser if first argument is given',
    function(done) {
      webAPIs.getBrowserKeys('Chrome').then((keys) => {
        expect(keys.sort()).toEqual([
          'Chrome_56.0.2924.87_OSX_10.12.2',
          'Chrome_55.123.45_OSX_10.12.2',
          'Chrome_55.321_Windows_10',
          'Chrome_55.321_Windows_8',
          'Chrome_55.321_Windows_7',
          ].sort());
        done();
      });
    });
    it(`returns keys for a specific browser and versions if two
    argument are given`, function(done) {
      webAPIs.getBrowserKeys('Chrome', '55').then((keys) => {
        expect(keys.sort()).toEqual([
          'Chrome_55.123.45_OSX_10.12.2',
          'Chrome_55.321_Windows_10',
          'Chrome_55.321_Windows_8',
          'Chrome_55.321_Windows_7',
          ].sort());
        done();
      });
    });
  });

  describe('toMap_()', function() {
    let webAPIs = com.web.api.WebAPIs.create();
    webAPIs.importAPI('Chrome', '55', 'Window', '10', {
      'Array': ['find'],
      'Audio': ['stop'],
    });
    webAPIs.importAPI('Edge', '14', 'Window', '10', {
      'Array': ['find'],
      'Audio': ['play'],
    });
    webAPIs.importAPI('Safari', '10', 'OSX', '601', {
      'ApplePay': ['about'],
      'Audio': ['play', 'stop'],
      'Array': ['find'],
    });

    describe('when all browser keys are selected', function() {
      it(`contains all browser keys in header array`, function(done) {
        webAPIs.browserAPIs.select().then((arrayDAO) => {
          webAPIs.toMap_(arrayDAO.a, [
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
      });
      it(`contains correct interface and API information`, function(done) {
        webAPIs.browserAPIs.select().then((arrayDAO) => {
          webAPIs.toMap_(arrayDAO.a, [
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
    });
    describe('when part of browser keys are selected', function() {
      it(`contains selected browser keys in header`, function() {
        webAPIs.browserAPIs.select().then((arrayDAO) => {
          webAPIs.toMap_(arrayDAO.a, [
            'Chrome_55_Window_10',
            'Edge_14_Window_10',
          ]).then((interfaceMap) => {
            expect(interfaceMap._header).toEqual([
              'Chrome_55_Window_10',
              'Edge_14_Window_10',
            ]);
          });
        });
      });
      it(`contains correct interface and API information`, function(done) {
        webAPIs.browserAPIs.where(M.IN(com.web.api.BrowserAPI.BROWSER_ID,
          [
            webAPIs.getBrowserId_('Chrome_55_Window_10'),
            webAPIs.getBrowserId_('Edge_14_Window_10'),
          ])).select()
        .then((arrayDAO) => {
          webAPIs.toMap_(arrayDAO.a, [
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

  describe('toMap()', function() {
    let webAPIs = com.web.api.WebAPIs.create();
    webAPIs.importAPI('Chrome', '55', 'Window', '10', {
      'Array': ['find'],
      'Audio': ['stop'],
    });
    webAPIs.importAPI('Edge', '14', 'Window', '10', {
      'Array': ['find'],
      'Audio': ['play'],
    });
    webAPIs.importAPI('Safari', '10', 'OSX', '601', {
      'ApplePay': ['about'],
      'Audio': ['play', 'stop'],
      'Array': ['find'],
    });
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

  describe('toCSV()', function() {
    let webAPIs = com.web.api.WebAPIs.create();
    webAPIs.importAPI('Chrome', '55', 'Window', '10', {
      'Array': ['find'],
      'Audio': ['stop'],
    });
    webAPIs.importAPI('Edge', '14', 'Window', '10', {
      'Array': ['find'],
      'Audio': ['play'],
    });
    webAPIs.importAPI('Safari', '10', 'OSX', '601', {
      'ApplePay': ['about'],
      'Audio': ['play', 'stop'],
      'Array': ['find'],
    });
    it(`produces correct csv string when all browsers are selected`,
    function(done) {
      webAPIs.toCSV([
        'Chrome_55_Window_10',
        'Edge_14_Window_10',
        'Safari_10_OSX_601',
      ]).then((csvStr) => {
        expect(csvStr).toEqual(
          'Interface,API,Chrome_55_Window_10,Edge_14_Window_10,' +
          'Safari_10_OSX_601\n' + 'Array,find,true,true,true\n' +
          'Audio,play,false,true,true\n' + 'Audio,stop,true,false,true\n' +
          'ApplePay,about,false,false,true\n'
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
