// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

describeLocal('data quality', function() {
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
    require('../../lib/datastore/datastore_container.es6.js');
    require('../../lib/web_apis/release.es6.js');
    require('../../lib/web_apis/release_interface_relationship.es6.js');
    require('../../lib/web_apis/web_interface.es6.js');

    var credentials = JSON.parse(fs.readFileSync(
        path.resolve(__dirname, '../../.local/credentials.json')));
    ctx = foam.lookup('org.chromium.apis.web.DatastoreContainer').create({
      gcloudAuthEmail: credentials.client_email,
      gcloudAuthPrivateKey: credentials.private_key,
      gcloudProjectId: credentials.project_id,
    }).ctx;

    Promise.all([
      ctx.releaseDAO.find('Chrome_56.0.2924.87_Windows_10.0')
          .then(function(release) { chrome = release; }),
      ctx.releaseDAO.find('Firefox_52.0_Windows_10.0')
          .then(function(release) { firefox = release; }),
      ctx.releaseDAO.find('Safari_602.4.8_OSX_10.12.3')
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
});
