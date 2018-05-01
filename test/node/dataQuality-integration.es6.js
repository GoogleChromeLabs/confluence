// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

if (global.isLocal) {
  jasmine.DEFAULT_TIMEOUT_INTERVAL = 60000;
}

xdescribe('data quality', function() {
  var E;
  var RWIJunction;
  var WebInterface;

  var ctx;

  var chrome;
  var firefox;
  var safari;
  var edge;

  beforeAll(function(done) {
    E = foam.lookup('foam.mlang.ExpressionsSingleton').create();
    RWIJunction =
        foam.lookup('org.chromium.apis.web.ReleaseWebInterfaceJunction');
    WebInterface = foam.lookup('org.chromium.apis.web.WebInterface');

    var fs = require('fs');
    var path = require('path');

    require('../../lib/confluence/api_velocity_data.es6.js');
    require('../../lib/confluence/browser_metric_data.es6.js');
    require('../../lib/dao/json_dao_container.es6.js');
    require('../../lib/web_apis/release.es6.js');
    require('../../lib/web_apis/release_interface_relationship.es6.js');
    require('../../lib/web_apis/web_interface.es6.js');
    const pkg = org.chromium.apis.web;

    ctx = pkg.JsonDAOContainer.create({
      mode: pkg.JsonDAOContainerMode.HTTP,
      basename: 'https://storage.googleapis.com/web-api-confluence-data-cache/latest/json',
    }).ctx;

    Promise.all([
      ctx.releaseDAO.find('Chrome_56.0.2924.87_Windows_10.0')
          .then(function(release) { chrome = release; }),
      ctx.releaseDAO.find('Firefox_52.0_Windows_10.0')
          .then(function(release) { firefox = release; }),
      ctx.releaseDAO.find('Safari_11.0_OSX_10.12.6')
          .then(function(release) { safari = release; }),
      ctx.releaseDAO.find('Edge_14.14393_Windows_10.0')
          .then(function(release) { edge = release; }),
    ]).then(done);
  });

  function expectAPILookup(shouldFind, release, api) {
    return ctx.releaseWebInterfaceJunctionDAO.where(
        E.AND(
            E.EQ(RWIJunction.SOURCE_ID, release.id),
            E.EQ(RWIJunction.TARGET_ID, api.id))).select()
        .then(function(sink) {
          if (sink.array.length !== (shouldFind ? 1 : 0)) {
            console.error('Expected', shouldFind ? '' : 'not', 'to find',
                          api.id, 'on', release.id);
          }
          expect(sink.array.length).toBe(shouldFind ? 1 : 0);
        });
  }
  function expectAPILookupAll(shouldFind, api) {
    return Promise.all([
      expectAPILookup(shouldFind, chrome, api),
      expectAPILookup(shouldFind, firefox, api),
      expectAPILookup(shouldFind, safari, api),
      expectAPILookup(shouldFind, edge, api),
    ]);
  }
  function mkAPI(ifName, apiName) {
    return WebInterface.create({
      interfaceName: ifName,
      apiName: apiName
    }, ctx);
  }

  it('should exclude constants such as CSSRule.CHARSET_RULE', function(done) {
    expectAPILookupAll(false, mkAPI('CSSRule', 'CHARSET_RULE'))
        .then(done, done.fail);
  });

  it('should include non-const ALL_CAPS such as Document.URL', function(done) {
    expectAPILookupAll(true, mkAPI('Document', 'URL'))
        .then(done, done.fail);
  });

  it(`should find instance-only APIs that are reachable from simple document
      window. E.g., no Window.FontFaceSet, but FontFaceSet.size still discovered
      in Chrome 56.`, function(done) {
    Promise.all([
      expectAPILookup(false, chrome, mkAPI('Window', 'FontFaceSet')),
      expectAPILookup(true, chrome, mkAPI('FontFaceSet', 'size')),
    ]).then(done, done.fail);
  });

  it(`should copy Firefox CSS2Properties into CSSStyleDeclaration. E.g.,
      CSSStyleDeclaration.border should exist`, function(done) {
    expectAPILookup(true, firefox, mkAPI('CSSStyleDeclaration', 'border'))
        .then(done, done.fail);
      });

  it(`should list exposed namespaces as interfaces. E.g., Window.Math,
      Math.abs.`, function(done) {
    Promise.all([
      expectAPILookupAll(true, mkAPI('Window', 'Math')),
      expectAPILookupAll(true, mkAPI('Math', 'abs')),
    ]).then(done, done.fail);
  });

  it(`should list exposed ordinary members, but not uninteresting members from
      constructor. E.g., Store MouseEvent.clientX, but not MouseEvent
      constructor's length, name, arguments, caller, prototype,
      constructor.`, function(done) {
    Promise.all([
      expectAPILookupAll(true, mkAPI('Window', 'MouseEvent')),
      expectAPILookupAll(true, mkAPI('MouseEvent', 'clientX')),
      expectAPILookupAll(false, mkAPI('MouseEvent', 'length')),
      expectAPILookupAll(false, mkAPI('MouseEvent', 'name')),
      expectAPILookupAll(false, mkAPI('MouseEvent', 'arguments')),
      expectAPILookupAll(false, mkAPI('MouseEvent', 'caller')),
      expectAPILookupAll(false, mkAPI('MouseEvent', 'prototype')),
      expectAPILookupAll(false, mkAPI('MouseEvent', 'constructor')),
    ]).then(done, done.fail);
  });

  it(`should list Function-related data on Function.`, function(done) {
    Promise.all([
      expectAPILookupAll(true, mkAPI('Window', 'Function')),
      expectAPILookupAll(true, mkAPI('Function', 'bind')),
      expectAPILookupAll(true, mkAPI('Function', 'length')),
      expectAPILookupAll(true, mkAPI('Function', 'name')),
      expectAPILookupAll(true, mkAPI('Function', 'arguments')),
      expectAPILookupAll(true, mkAPI('Function', 'caller')),
    ]).then(done, done.fail);
  });

  it(`should list members from prototype with the same name as uninteresting
      members from the constructor. E.g.,
      Window.prototype.name.`, function(done) {
    expectAPILookup(true, firefox, mkAPI('Window', 'name'))
        .then(done, done.fail);

    // NOTE: Not expectAPILookupAll() because some implementations (including
    //       Chrome 56) implement `window.name` as a constant on `window`, and
    //       constants are filtered out of the API catalog.
  });
});
