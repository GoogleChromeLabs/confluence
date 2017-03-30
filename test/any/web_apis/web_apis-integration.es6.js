// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

/*
 * The object graph files under /test/data are extracted using
 * object-graph-js (https://github.com/mdittmer/object-graph-js). The test
 * specifications are defined in the API Result test cases sheet.
 **/

describe('WebAPI and api extractor', function() {
  let webCatalog;
  beforeEach(function(done) {
    let chrome56 = global.DATA.chrome56;
    let edge14 = global.DATA.edge14;
    let safari602 = global.DATA.safari602;
    let og = global.ObjectGraph;
    let extractor = org.chromium.apis.web.apiExtractor.create({});
    let apiImporter = org.chromium.apis.web.ApiImporter.create();
    let apiMatrix = org.chromium.apis.web.ApiMatrix.create({
      browserApiDAO: apiImporter.browserApiDAO,
      browserDAO: apiImporter.browserDAO,
      interfaceDAO: apiImporter.interfaceDAO,
    });

    apiImporter.import('Chrome', '56', 'Windows', '10',
      extractor.extractWebCatalog(og.fromJSON(chrome56)));
    apiImporter.import('Edge', '14.14393', 'Windows', '10',
        extractor.extractWebCatalog(og.fromJSON(edge14)));
    apiImporter.import('Safari', '602.4.8', 'OSX', '10',
        extractor.extractWebCatalog(og.fromJSON(safari602)));
    apiMatrix.toMatrix(['Chrome_56_Windows_10', 'Edge_14.14393_Windows_10',
    'Safari_602.4.8_OSX_10']).then((webCatalogMatrix) =>{
      webCatalog = webCatalogMatrix;
      done();
    });
  });

  it('filters out constant primitive properties', function() {
    expect(webCatalog.CSSRule.CHARSET_RULE).toBeUndefined();
    expect(webCatalog.Math.PI).toBeUndefined();
  });
  it('contains capital non-constant properties', function() {
    // Constant value are not identified as all-capital value.
    expect(webCatalog.Document.URL).toEqual({
      'Chrome_56_Windows_10': true,
      'Edge_14.14393_Windows_10': true,
      'Safari_602.4.8_OSX_10': true,
    });
    expect(webCatalog.BiquadFilterNode.Q).toEqual({
      'Chrome_56_Windows_10': true,
      'Edge_14.14393_Windows_10': true,
      'Safari_602.4.8_OSX_10': true,
    });
  });
  describe('Window interface', function() {
    it(`contains first level objects`, function() {
      expect(webCatalog.Window.alert).toEqual({
        'Chrome_56_Windows_10': true,
        'Edge_14.14393_Windows_10': true,
        'Safari_602.4.8_OSX_10': true,
      });
      expect(webCatalog.Window.Boolean).toEqual({
        'Chrome_56_Windows_10': true,
        'Edge_14.14393_Windows_10': true,
        'Safari_602.4.8_OSX_10': true,
      });
      expect(webCatalog.Window.ApplePaySession).toEqual({
        'Safari_602.4.8_OSX_10': true,
      });
      expect(webCatalog.Window.Math).toEqual({
        'Chrome_56_Windows_10': true,
        'Edge_14.14393_Windows_10': true,
        'Safari_602.4.8_OSX_10': true,
      });
      expect(webCatalog.Window.MouseEvent).toEqual({
        'Chrome_56_Windows_10': true,
        'Edge_14.14393_Windows_10': true,
        'Safari_602.4.8_OSX_10': true,
      });
    });
    it(`contains first level objects that references to the same object
      as separate interfaecs`, function() {
        // In chrome, MediaStream is the same function as webkitMediaStream.
        expect(webCatalog.Window.MediaStream).toBeDefined();
        expect(webCatalog.Window.webkitMediaStream).toBeDefined();
      });
    it('does not contain non-interface objects', function() {
      // Chrome does not expose FontFace as global interface.
      expect(webCatalog.Window.FontFaceSet.
        Chrome_56_Windows_10).toBeUndefined;
    });
  });
  it('ignores built-in function properties on function instances',
    function() {
      expect(webCatalog.MouseEvent.name).toBeUndefined();
      expect(webCatalog.MouseEvent.caller).toBeUndefined();
      expect(webCatalog.MouseEvent.bind).toBeUndefined();
      expect(webCatalog.AnalyserNode.caller).toBeUndefined();
      expect(webCatalog.AnalyserNode.name).toBeUndefined();
      expect(webCatalog.AnalyserNode.bind).toBeUndefined();
    });
  it('captures built-in properties for Function and Object',
    function() {
      expect(webCatalog.Function.bind).toEqual({
        'Chrome_56_Windows_10': true,
        'Edge_14.14393_Windows_10': true,
        'Safari_602.4.8_OSX_10': true,
      });
      expect(webCatalog.Function.apply).toEqual({
        'Chrome_56_Windows_10': true,
        'Edge_14.14393_Windows_10': true,
        'Safari_602.4.8_OSX_10': true,
      });
      expect(webCatalog.Function.call).toEqual({
        'Chrome_56_Windows_10': true,
        'Edge_14.14393_Windows_10': true,
        'Safari_602.4.8_OSX_10': true,
      });
      expect(webCatalog.Function.length).toEqual({
        'Chrome_56_Windows_10': true,
        'Edge_14.14393_Windows_10': true,
        'Safari_602.4.8_OSX_10': true,
      });
      expect(webCatalog.Function.name).toEqual({
        'Chrome_56_Windows_10': true,
        'Edge_14.14393_Windows_10': true,
        'Safari_602.4.8_OSX_10': true,
      });
      expect(webCatalog.Object.__defineGetter__).toEqual({
        'Chrome_56_Windows_10': true,
        'Edge_14.14393_Windows_10': true,
        'Safari_602.4.8_OSX_10': true,
      });
      expect(webCatalog.Object.hasOwnProperty).toEqual({
        'Chrome_56_Windows_10': true,
        'Edge_14.14393_Windows_10': true,
        'Safari_602.4.8_OSX_10': true,
      });
      expect(webCatalog.Object.toString).toEqual({
        'Chrome_56_Windows_10': true,
        'Edge_14.14393_Windows_10': true,
        'Safari_602.4.8_OSX_10': true,
      });
      expect(webCatalog.Object.constructor).toEqual({
        'Chrome_56_Windows_10': true,
        'Edge_14.14393_Windows_10': true,
        'Safari_602.4.8_OSX_10': true,
      });
    });
});
