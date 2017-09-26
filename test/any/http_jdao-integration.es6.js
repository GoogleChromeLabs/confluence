// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

describe('HttpJDAO', function() {
  let HttpJDAO;
  let MDAO;

  beforeEach(function() {
    HttpJDAO = foam.lookup('org.chromium.apis.web.HttpJDAO');
    MDAO = foam.lookup('foam.dao.MDAO');
  });

  function testDAO(clsId) {
    it(`should load ${clsId} DAO`, function(done) {
      const cls = foam.lookup(clsId);
      const dao = HttpJDAO.create({
        of: cls,
        delegate: MDAO.create({of: cls}),
        url: `https://storage.googleapis.com/web-api-confluence-data-cache/latest/journal/${clsId}-journal.js`,
      });
      dao.select().then(function(arraySink) {
        expect(arraySink.array.length).toBeGreaterThan(0);
      }).then(done, done.fail);
    });
  }

  testDAO('org.chromium.apis.web.VersionedRelease');

  // These data sets are big. Do not include in automated tests.
  // testDAO('org.chromium.apis.web.VersionedWebInterface');
  // testDAO('org.chromium.apis.web.VersionedReleaseWebInterfaceJunction');

  testDAO('org.chromium.apis.web.VersionedApiVelocityData');
  testDAO('org.chromium.apis.web.VersionedBrowserMetricData');
});
