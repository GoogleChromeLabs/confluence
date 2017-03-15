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
  describe('toMatrix()', function() {
    let apiImporter = org.chromium.apis.web.ApiImporter.create();
    let apiMatrix = org.chromium.apis.web.ApiMatrix.create({
      browserAPIs: apiImporter.browserAPIs,
    });
    apiImporter.import('Chrome', '55', 'Window', '10', {
      'Array': ['find'],
      'Audio': ['stop'],
    });
    apiImporter.import('Edge', '14', 'Window', '10', {
      'Array': ['find'],
      'Audio': ['play'],
    });
    apiImporter.import('Safari', '10', 'OSX', '601', {
      'ApplePay': ['about'],
      'Audio': ['play', 'stop'],
      'Array': ['find'],
    });
    it(`contains correct interface and API information, when all browsers
    are selected`, function(done) {
      apiMatrix.toMatrix([
        'Chrome_55_Window_10',
        'Edge_14_Window_10',
        'Safari_10_OSX_601',
      ]).then((interfaceMap) => {
        expect(interfaceMap.ApplePay.about).toEqual({
          Safari_10_OSX_601: true,
        });
        expect(interfaceMap.Array.find).toEqual({
          Chrome_55_Window_10: true,
          Edge_14_Window_10: true,
          Safari_10_OSX_601: true,
        });
        expect(interfaceMap.Audio.play).toEqual({
          Edge_14_Window_10: true,
          Safari_10_OSX_601: true,
        });
        expect(interfaceMap.Audio.stop).toEqual({
          Chrome_55_Window_10: true,
          Safari_10_OSX_601: true,
        });
        done();
      });
    });
    it(`contains correct interface and API information, when part of
    keys are selected.`, function(done) {
      apiMatrix.toMatrix([
        'Chrome_55_Window_10',
        'Edge_14_Window_10',
      ]).then((interfaceMap) => {
        expect(interfaceMap.Array.find).toEqual({
          Chrome_55_Window_10: true,
          Edge_14_Window_10: true,
        });
        expect(interfaceMap.Audio.play).toEqual({
          Edge_14_Window_10: true,
        });
        expect(interfaceMap.Audio.stop).toEqual({
          Chrome_55_Window_10: true,
        });
        done();
      });
    });
  });

  describe('toCSV()', function() {
    let apiImporter = org.chromium.apis.web.ApiImporter.create();
    let apiMatrix = org.chromium.apis.web.ApiMatrix.create({
      browserAPIs: apiImporter.browserAPIs,
    });
    apiImporter.import('Chrome', '55', 'Window', '10', {
      'Array': ['find'],
      'Audio': ['stop'],
    });
    apiImporter.import('Edge', '14', 'Window', '10', {
      'Array': ['find'],
      'Audio': ['play'],
    });
    apiImporter.import('Safari', '10', 'OSX', '601', {
      'ApplePay': ['about'],
      'Audio': ['play', 'stop'],
      'Array': ['find'],
    });
    it(`produces correct csv string when all browsers are selected`,
    function(done) {
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
        done();
      });
    });
    it(`produces correct csv string when part of browsers are selected`,
    function(done) {
      apiMatrix.toCSV([
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
