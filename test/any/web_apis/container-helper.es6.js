// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

beforeAll(function() {
  global.getReleaseApiContainer = function(opt_ctx) {
    var ctx = opt_ctx || foam.__context__;
    var EasyDAO = ctx.lookup('foam.dao.EasyDAO');
    var ReleaseApiContainer =
        ctx.lookup('org.chromium.apis.web.ReleaseApiContainer');
    var Release = ctx.lookup('org.chromium.apis.web.Release');
    var WebInterface = ctx.lookup('org.chromium.apis.web.WebInterface');
    var ReleaseWebInterfaceJunction =
        ctx.lookup('org.chromium.apis.web.ReleaseWebInterfaceJunction');

    return ReleaseApiContainer.create({
      releaseDAO: EasyDAO.create({
        name: 'releaseDAO',
        of: Release,
        daoType: 'MDAO',
      }),
      webInterfaceDAO: EasyDAO.create({
        name: 'webInterfaceDAO',
        of: WebInterface,
        daoType: 'MDAO',
      }),
      releaseWebInterfaceJunctionDAO: EasyDAO.create({
        name: 'releaseWebInterfaceJunctionDAO',
        of: ReleaseWebInterfaceJunction,
        daoType: 'MDAO',
      }),
    });
  };
});
