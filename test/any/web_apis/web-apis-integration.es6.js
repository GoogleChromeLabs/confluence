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

/*
 * The object graph files under /test/data are extracted using
 * web-apis (https://github.com/mdittmer/web-apis). The test
 * specifications are defined at:
 * https://docs.google.com/spreadsheets/d/1bnm-atGD2SPyQ5XBltEXOaVhzK0ey5Dvpe4Nwu2m588
 **/

describe('WebAPI and api extractor', function() {
  let chrome56 = global.DATA.chrome56;
  let edge14 = global.DATA.edge14;
  let safari602 = global.DATA.safari602;
  let og = global.ObjectGraph;
  let extractor = org.chromium.apis.web.apiExtractor.create({});
  let webAPIs = org.chromium.apis.web.WebAPIs.create();

  webAPIs.importAPI('Chrome', '56', 'Windows', '10',
    extractor.extractWebCatalog(og.fromJSON(chrome56)))
  .then(() => {
    return webAPIs.importAPI('Edge', '14', 'Windows', '10',
      extractor.extractWebCatalog(og.fromJSON(edge14)));
  }).then(() => {
    return webAPIs.importAPI('Safari', '602', 'OSX', '10',
      extractor.extractWebCatalog(og.fromJSON(safari602)));
  }).then(() => {
    webAPIs.toMap(['Chrome_56_Windows_10', 'Edge_14_Windows_10',
    'Safari_602_OSX_10']).then((webCatalogMap) => {
      it('filters out constant primitive properties', function(done) {
        expect(webCatalogMap.CSSRule.CHARSET_RULE).toBeUndefined();
        expect(webCatalogMap.Math.PI).toBeUndefined();
        done();
      });
      it('contains capital non-constant properties', function(done) {
        // Constant value are not identified as all-capital value.
        expect(webCatalogMap.Document.URL).toEqual([true, true, true]);
        expect(webCatalogMap.BiquadFilterNode.Q).toEqual([true, true, true]);
        done();
      });
      describe('Window interface', function() {
        it(`contains first level objects`, function(done) {
          // Constant value are not identified as all-capital value.
          expect(webCatalogMap.Window.alert).toEqual([true, true, true]);
          expect(webCatalogMap.Window.Boolean).toEqual([true, true, true]);
          expect(webCatalogMap.Window.ApplePaySession)
            .toEqual([false, false, true]);
          expect(webCatalogMap.Window.Math).toEqual([true, true, true]);
          expect(webCatalogMap.Window.MouseEvent).toEqual([true, true, true]);
          done();
        });
        it('does not contain non-interface object', function(done) {
          // Chrome does not expose FontFase as global interface.
          expect(webCatalogMap.Window.FontFaceSet[0]).toBe(false);
          done();
        });
      });
      it('filters out built-in function object properties', function(done) {
        expect(webCatalogMap.MouseEvent.name).toBeUndefined();
        expect(webCatalogMap.MouseEvent.caller).toBeUndefined();
        expect(webCatalogMap.MouseEvent.bind).toBeUndefined();
        // toBeUndefined does not work in checking prototype and constructor
        // since they are built-in for JSON objects.
        expect(webCatalogMap.MouseEvent.prototype)
          .not.toEqual([true, true, true]);
        expect(webCatalogMap.MouseEvent.constructor)
          .not.toEqual([true, true, true]);
        expect(webCatalogMap.AnalyserNode.caller).toBeUndefined();
        expect(webCatalogMap.AnalyserNode.name).toBeUndefined();
        expect(webCatalogMap.AnalyserNode.bind).toBeUndefined();
        expect(webCatalogMap.AnalyserNode.prototype)
          .not.toEqual([true, true, true]);
        expect(webCatalogMap.AnalyserNode.constructor)
          .not.toEqual([true, true, true]);
        done();
      });
      it('captures built-in properties for Function and Object',
      function(done) {
        expect(webCatalogMap.Function.bind).toEqual([true, true, true]);
        expect(webCatalogMap.Function.apply).toEqual([true, true, true]);
        expect(webCatalogMap.Function.call).toEqual([true, true, true]);
        expect(webCatalogMap.Function.length).toEqual([true, true, true]);
        expect(webCatalogMap.Function.name).toEqual([true, true, true]);
        expect(webCatalogMap.Object.__defineGetter__).toEqual([true, true, true]);
        expect(webCatalogMap.Object.hasOwnProperty).toEqual([true, true, true]);
        expect(webCatalogMap.Object.toString).toEqual([true, true, true]);
        expect(webCatalogMap.Object.constructor).toEqual([true, true, true]);
        done();
      });
    });
  });
});
