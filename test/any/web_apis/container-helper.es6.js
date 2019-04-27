// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

beforeAll(function() {
  global.createDAOContainer = function(opt_ctx) {
    const E = foam.mlang.ExpressionsSingleton.create();
    const ctx = (opt_ctx || foam.__context__).createSubContext({
      releasePredicate: E.EQ(
          foam.lookup('org.chromium.apis.web.Release').IS_MOBILE,
          false),
    });
    const EasyDAO = ctx.lookup('foam.dao.EasyDAO');
    const DAOContainer =
        ctx.lookup('org.chromium.apis.web.DAOContainer');
    const Release = ctx.lookup('org.chromium.apis.web.Release');
    const WebInterface = ctx.lookup('org.chromium.apis.web.WebInterface');
    const ReleaseWebInterfaceJunction =
        ctx.lookup('org.chromium.apis.web.ReleaseWebInterfaceJunction');
    const CompatData =
        ctx.lookup('org.chromium.apis.web.generated.CompatData');
    const BrowserMetricData =
        ctx.lookup('org.chromium.apis.web.BrowserMetricData');
    const ApiCountData =
        ctx.lookup('org.chromium.apis.web.ApiCountData');

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
      compatDAO: EasyDAO.create({
        name: 'compatDAO',
        of: CompatData,
        daoType: 'MDAO',
      }),
      browserMetricsDAO: EasyDAO.create({
        name: 'browserMetricsDAO',
        of: BrowserMetricData,
        daoType: 'MDAO',
      }),
      apiCountDAO: EasyDAO.create({
        name: 'apiCountDAO',
        of: ApiCountData,
        daoType: 'MDAO',
      }),
    }, ctx);
  };
});
