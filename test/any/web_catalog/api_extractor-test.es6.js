// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';


// NOTE: These are not really unit tests because they use the "real" ObjectGraph
// interface. They are closer to unit tests than tests in
// api_extractor-integration because they use small, locally generated object
// graphs.

describe('API extractor', () => {
  let ObjectGraph;
  let ApiExtractor;

  const getCatalog = (root, ogConfig, extractorConfig) => {
    return new Promise((resolve, reject) => {
      const og = new ObjectGraph({
        onDone: () => {
          extractorConfig.objectGraph = og;
          resolve(ApiExtractor.create(extractorConfig).extractWebCatalog(og));
        },
      });
      og.capture(root, ogConfig);
    });
  };

  beforeAll(() => {
    ObjectGraph = global.ObjectGraph;
  });

  // Instantiate data related to FOAM classes each time (in each test context).
  beforeEach(() => {
    ApiExtractor = org.chromium.apis.web.ApiExtractor;

    // TODO(markdittmer): object-graph-js currentlyu uses "window.setTimeout()".
    // This should be fixed upstream to eliminate the need for this hack.
    if (typeof window === 'undefined') global.window = {setTimeout};
  });

  it('should expose Object interface', done => {
    getCatalog({Object}, {key: 'window'}, {})
        .then(catalog => {
          for (const key in Object) {
            if (!Object.hasOwnProperty(key)) continue;
            expect(catalog.Object.includes(key)).toBe(true);
          }
          for (const key in Object.prototype) {
            if (!Object.prototype.hasOwnProperty(key)) continue;
            expect(catalog.Object.includes(key)).toBe(true);
          }
        }).then(done, done.fail);
  });

  it('should expose Function interface', done => {
    getCatalog({Function}, {key: 'window'}, {})
        .then(catalog => {
          for (const key in Function) {
            if (!Function.hasOwnProperty(key)) continue;
            expect(catalog.Function.includes(key)).toBe(true);
          }
          for (const key in Function.prototype) {
            if (!Function.prototype.hasOwnProperty(key)) continue;
            expect(catalog.Function.includes(key)).toBe(true);
          }
        }).then(done, done.fail);
  });

  it('should expose method', done => {
    function Alpha() {}
    Alpha.prototype.beta = function beta() {};
    Promise.all([
      getCatalog({Object, Function}, {key: 'window'}, {}),
      getCatalog({Object, Function, Alpha}, {key: 'window'}, {}),
    ]).then(baseAndAlpha => {
      const baseCatalog = baseAndAlpha[0];
      const alphaCatalog = baseAndAlpha[1];
      expect(baseCatalog.Object.sort()).toEqual(alphaCatalog.Object.sort());
      expect(baseCatalog.Function.sort()).toEqual(alphaCatalog.Function.sort());
      expect(alphaCatalog.Alpha.sort()).toEqual(['beta'].sort());
      expect(Object.keys(alphaCatalog).length).toBe(3);
    }).then(done, done.fail);
  });

  it('should expose null-property', done => {
    function Alpha() {}
    Alpha.prototype.beta = null;
    Promise.all([
      getCatalog({Object, Function}, {key: 'window'}, {}),
      getCatalog({Object, Function, Alpha}, {key: 'window'}, {}),
    ]).then(baseAndAlpha => {
      const baseCatalog = baseAndAlpha[0];
      const alphaCatalog = baseAndAlpha[1];
      expect(baseCatalog.Object.sort()).toEqual(alphaCatalog.Object.sort());
      expect(baseCatalog.Function.sort()).toEqual(alphaCatalog.Function.sort());
      expect(alphaCatalog.Alpha.sort()).toEqual(['beta'].sort());
      expect(Object.keys(alphaCatalog).length).toBe(3);
    }).then(done, done.fail);
  });

  it('should expose function-like object as interface', done => {
    let Alpha = {prototype: {}};
    Alpha.prototype.beta = function beta() {};
    Alpha.prototype.charlie = null;
    Promise.all([
      getCatalog({Object, Function}, {key: 'window'}, {}),
      getCatalog({Object, Function, Alpha}, {key: 'window'}, {}),
    ]).then(baseAndAlpha => {
      const baseCatalog = baseAndAlpha[0];
      const alphaCatalog = baseAndAlpha[1];
      expect(baseCatalog.Object.sort()).toEqual(alphaCatalog.Object.sort());
      expect(baseCatalog.Function.sort()).toEqual(alphaCatalog.Function.sort());
      expect(alphaCatalog.Alpha.sort()).toEqual(['beta', 'charlie'].sort());
      expect(Object.keys(alphaCatalog).length).toBe(3);
    }).then(done, done.fail);
  });

  it('should expose properties from instances and libraries', done => {
    function Alpha() {}
    Alpha.prototype.beta = null;
    let alpha = new Alpha();
    alpha.charlie = function charlie() {};
    let superAlpha = Object.create(alpha);
    superAlpha.delta = {};
    const alphaLib = {alpha, superAlpha};
    Promise.all([
      getCatalog({Object, Function}, {key: 'window'}, {}),
      getCatalog({Object, Function, Alpha, alphaLib}, {key: 'window'}, {}),
    ]).then(baseAndAlpha => {
      const baseCatalog = baseAndAlpha[0];
      const alphaCatalog = baseAndAlpha[1];
      expect(baseCatalog.Object.sort()).toEqual(alphaCatalog.Object.sort());
      expect(baseCatalog.Function.sort()).toEqual(alphaCatalog.Function.sort());
      expect(alphaCatalog.Alpha.sort())
          .toEqual(['beta', 'charlie', 'delta'].sort());
      expect(alphaCatalog.alphaLib.sort())
          .toEqual(['alpha', 'superAlpha'].sort());
      expect(Object.keys(alphaCatalog).length).toBe(4);
    }).then(done, done.fail);
  });

  it('should expose properties from "hidden" prototypes', done => {
    function Alpha() {}
    function Beta() {}
    Alpha.prototype = Object.create({alpha: null});
    let hiddenPrototype = Object.create(Alpha.prototype);
    hiddenPrototype.charlie = function charlie() {};
    Beta.prototype = Object.create(hiddenPrototype);
    Beta.prototype.beta = function beta() {};
    Promise.all([
      getCatalog({Object, Function}, {key: 'window'}, {}),
      getCatalog({Object, Function, Alpha, Beta}, {key: 'window'}, {}),
    ]).then(baseAndAlphaBeta => {
      const baseCatalog = baseAndAlphaBeta[0];
      const alphaBetaCatalog = baseAndAlphaBeta[1];
      expect(baseCatalog.Object.sort()).toEqual(alphaBetaCatalog.Object.sort());
      expect(baseCatalog.Function.sort())
          .toEqual(alphaBetaCatalog.Function.sort());
      expect(alphaBetaCatalog.Alpha.sort())
          .toEqual(['alpha'].sort());
      expect(alphaBetaCatalog.Beta.sort())
          .toEqual(['beta', 'charlie'].sort());
      expect(Object.keys(alphaBetaCatalog).length).toBe(4);
    }).then(done, done.fail);
  });

  it('should deduce function names from graph paths', done => {
    // Ensure that Alpha.name is not automatically assigned 'Alpha'.
    const Alpha = (function() { return function() {}; })();
    // Prevent Alpha.prototype.constructor === Alpha.
    Alpha.prototype = {};
    Alpha.alpha = null;
    const lib = {property: {Alpha}};
    Promise.all([
      getCatalog({Object, Function}, {key: 'window'}, {}),
      getCatalog({Object, Function, lib}, {key: 'window'}, {}),
      getCatalog({Object, Function, lib}, {key: 'window'},
                 {functionNamesFromGraphPaths: false}),
    ]).then(catalogs => {
      const baseCatalog = catalogs[0];
      const libCatalog = catalogs[1];
      const libNoAlphaCatalog = catalogs[2];

      expect(baseCatalog.Object.sort()).toEqual(libCatalog.Object.sort());
      expect(baseCatalog.Function.sort()).toEqual(libCatalog.Function.sort());
      expect(libCatalog.lib.sort()).toEqual(['property'].sort());
      expect(libCatalog.Alpha.sort()).toEqual(['alpha'].sort());
      expect(Object.keys(libCatalog).length).toBe(4);

      expect(baseCatalog.Object.sort())
          .toEqual(libNoAlphaCatalog.Object.sort());
      expect(baseCatalog.Function.sort())
          .toEqual(libNoAlphaCatalog.Function.sort());
      expect(libNoAlphaCatalog.lib.sort()).toEqual(['property'].sort());
      expect(Object.keys(libNoAlphaCatalog).length).toBe(3);
    }).then(done, done.fail);
  });

  it('should skip constants', done => {
    function Alpha() {}
    Alpha.INT_CONSTANT = 0;
    Alpha.prototype.STRING_CONSTANT = '';
    Alpha.prototype.BOOLEAN_CONSTANT = false;
    Promise.all([
      getCatalog({Object, Function}, {key: 'window'}, {}),
      getCatalog({Object, Function, Alpha}, {key: 'window'}, {}),
      getCatalog({Object, Function, Alpha}, {key: 'window'},
                 {constantTypes: []}),
    ]).then(catalogs => {
      const baseCatalog = catalogs[0];
      const alphaCatalog = catalogs[1];
      const alphaWithConstantsCatalog = catalogs[2];

      expect(baseCatalog.Object.sort()).toEqual(alphaCatalog.Object.sort());
      expect(baseCatalog.Function.sort()).toEqual(alphaCatalog.Function.sort());
      expect(alphaCatalog.Alpha).toBeUndefined();
      expect(Object.keys(alphaCatalog).length).toBe(2);

      expect(baseCatalog.Object.sort())
          .toEqual(alphaWithConstantsCatalog.Object.sort());
      expect(baseCatalog.Function.sort())
          .toEqual(alphaWithConstantsCatalog.Function.sort());
      expect(alphaWithConstantsCatalog.Alpha.sort()).toEqual([
        'INT_CONSTANT',
        'STRING_CONSTANT',
        'BOOLEAN_CONSTANT',
      ].sort());
      expect(Object.keys(alphaWithConstantsCatalog).length).toBe(3);
    }).then(done, done.fail);
  });

  it('should combine multiple instances and prototypes without duplicates', done => {
    function Alpha() {}
    Alpha.prototype.alpha = function alpha() {};
    let superAlpha = Object.create(Alpha.prototype);
    superAlpha.alpha = function alpha() {};
    superAlpha.superAlpha = function superAlpha() {};
    let superDuperAlpha = Object.create(superAlpha);
    superDuperAlpha.alpha = function alpha() {};
    superDuperAlpha.superAlpha = function superAlpha() {};
    superDuperAlpha.superDuperAlpha = function superDuperAlpha() {};
    function Beta() {}
    Beta.prototype = Object.create(superAlpha);
    Beta.prototype.constructor = Beta;
    Beta.prototype.alpha = function alpha() {};
    Beta.prototype.superAlpha = function superAlpha() {};
    Beta.prototype.superDuperAlpha = function superDuperAlpha() {};
    Beta.prototype.beta = function beta() {};
    let beta = new Beta();
    beta.alpha = function alpha() {};
    beta.superAlpha = function superAlpha() {};
    beta.superDuperAlpha = function superDuperAlpha() {};
    beta.beta = function beta() {};
    beta.instanceProperty = null;
    let superBeta = Object.create(beta);
    superBeta.alpha = function alpha() {};
    superBeta.superAlpha = function superAlpha() {};
    superBeta.superDuperAlpha = function superDuperAlpha() {};
    superBeta.beta = function beta() {};
    superBeta.instanceProperty = null;
    superBeta.superProperty = '';

    Promise.all([
      getCatalog({Object, Function}, {key: 'window'}, {}),
      getCatalog({Object, Function, lib: {superBeta}}, {key: 'window'}, {}),
    ]).then(catalogs => {
      const baseCatalog = catalogs[0];
      const catalog = catalogs[1];
      expect(baseCatalog.Object.sort()).toEqual(catalog.Object.sort());
      expect(baseCatalog.Function.sort()).toEqual(catalog.Function.sort());
      expect(catalog.lib).toEqual(['superBeta'].sort());
      expect(catalog.Alpha.sort()).toEqual(['alpha'].sort());
      expect(catalog.Beta.sort())
          .toEqual([
            'superProperty',
            'instanceProperty',
            'beta',
            'superDuperAlpha',
            'superAlpha',
          ].sort());
      expect(Object.keys(catalog).length).toBe(5);
    }).then(done, done.fail);
  });

  it('should combine non-built-in prototypes on libraries', done => {
    function method1() {};
    let libProto = {method1};
    function method2() {};
    let lib = Object.create(libProto, {method2: {value: method2}});
    Promise.all([
      getCatalog({Object, Function}, {key: 'window'}, {}),
      getCatalog({Object, Function, lib}, {key: 'window'}, {}),
    ]).then(catalogs => {
      const baseCatalog = catalogs[0];
      const catalog = catalogs[1];
      expect(baseCatalog.Object.sort()).toEqual(catalog.Object.sort());
      expect(baseCatalog.Function.sort()).toEqual(catalog.Function.sort());
      expect(catalog.lib.sort()).toEqual(['method1', 'method2'].sort());
      expect(Object.keys(catalog).length).toBe(3);
    }).then(done, done.fail);
  });
});
