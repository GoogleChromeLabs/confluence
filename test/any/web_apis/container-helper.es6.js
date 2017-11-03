// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

beforeAll(function() {
  global.createDAOContainer = function(opt_ctx) {
    var E = foam.mlang.ExpressionsSingleton.create();
    var ctx = (opt_ctx || foam.__context__).createSubContext({
      releasePredicate: E.EQ(
          foam.lookup('org.chromium.apis.web.Release').IS_MOBILE,
          false),
    });
    var EasyDAO = ctx.lookup('foam.dao.EasyDAO');
    var DAOContainer =
        ctx.lookup('org.chromium.apis.web.DAOContainer');
    var Release = ctx.lookup('org.chromium.apis.web.Release');
    var WebInterface = ctx.lookup('org.chromium.apis.web.WebInterface');
    var ReleaseWebInterfaceJunction =
        ctx.lookup('org.chromium.apis.web.ReleaseWebInterfaceJunction');
    var BrowserMetricData =
        ctx.lookup('org.chromium.apis.web.BrowserMetricData');
    var ApiVelocityData =
        ctx.lookup('org.chromium.apis.web.ApiVelocityData');

    return DAOContainer.create({
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
      browserMetricsDAO: EasyDAO.create({
        name: 'browserMetricsDAO',
        of: BrowserMetricData,
        daoType: 'MDAO',
      }),
      apiVelocityDAO: EasyDAO.create({
        name: 'apiVelocityDAO',
        of: ApiVelocityData,
        daoType: 'MDAO',
      }),
    }, ctx);
  };
});
