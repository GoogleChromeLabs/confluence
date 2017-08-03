// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

const fs = require('fs');
const path = require('path');

global.FOAM_FLAGS = {gcloud: true};
require('foam2');

require('../lib/confluence/aggressive_removal.es6.js');
require('../lib/confluence/api_velocity.es6.js');
require('../lib/confluence/browser_specific.es6.js');
require('../lib/confluence/failure_to_ship.es6.js');
require('../lib/datastore/datastore_container.es6.js');
require('../lib/web_apis/release.es6.js');
require('../lib/web_apis/release_interface_relationship.es6.js');
require('../lib/web_apis/web_interface.es6.js');

let server = foam.lookup('foam.net.node.Server').create({
  port: 8080,
});

const credentials = JSON.parse(fs.readFileSync(
    path.resolve(__dirname, '../.local/credentials.json')));
const logger = foam.lookup('foam.log.ConsoleLogger').create();
const daoContainer =
    foam.lookup('org.chromium.apis.web.DatastoreContainer').create({
      gcloudAuthEmail: credentials.client_email,
      gcloudAuthPrivateKey: credentials.private_key,
      gcloudProjectId: credentials.project_id,
      logger: logger,
    });
const ctx = daoContainer.ctx
    .createSubContext(foam.lookup('foam.box.Context').create());

server.exportFile('/', `${__dirname}/../static/index.html`);
server.exportDirectory('/images', `${__dirname}/../static/images`);
server.exportDirectory('/', `${__dirname}/../static/bundle`);

// TODO(markdittmer): Unify this script by adding support for box-registered
// DAOs to foam.net.node.Server.

// Register DAOs in box context. Wrap DAOs in ReadOnlyDAO for security and
// LazyCacheDAO for performance.
//
// TODO(markdittmer): Customize MDAO indexes according to common queries.
const ReadOnlyDAO = foam.lookup('foam.dao.ReadOnlyDAO');
const LazyCacheDAO = foam.lookup('foam.dao.LazyCacheDAO');
const MDAO = foam.lookup('foam.dao.MDAO');
const SkeletonBox = foam.lookup('foam.box.SkeletonBox');
function registerDAO(name, dao) {
  foam.assert(dao, 'Broken use of helper: registerDAO()');
  var daoToRegister = foam.dao.ReadOnlyDAO.create({
    delegate: LazyCacheDAO.create({
      cacheOnSelect: true,
      staleTimeout: 1000 * 60 * 60 * 24,
      cache: MDAO.create({ of: dao.of }, ctx),
      delegate: dao,
    }, ctx)
  }, ctx);
  ctx.registry.register(name, null, SkeletonBox.create({
    data: daoToRegister
  }, ctx));
}

registerDAO(daoContainer.RELEASE_NAME, ctx.releaseDAO);
registerDAO(daoContainer.WEB_INTERFACE_NAME, ctx.webInterfaceDAO);
registerDAO(daoContainer.RELEASE_WEB_INTERFACE_JUNCTION_NAME,
            ctx.releaseWebInterfaceJunctionDAO);
registerDAO(daoContainer.API_VELOCITY_NAME, ctx.apiVelocityDAO);

const E = foam.lookup('foam.mlang.ExpressionsSingleton').create();
const EQ = E.EQ.bind(E);
const Type = foam.lookup('org.chromium.apis.web.BrowserMetricDataType');
const TYPE = foam.lookup('org.chromium.apis.web.BrowserMetricData').TYPE;

registerDAO(
    daoContainer.FAILURE_TO_SHIP_NAME,
    ctx.browserMetricsDAO.where(EQ(TYPE, Type.FAILURE_TO_SHIP)));
registerDAO(
    daoContainer.BROWSER_SPECIFIC_NAME,
    ctx.browserMetricsDAO.where(EQ(TYPE, Type.BROWSER_SPECIFIC)));
registerDAO(
    daoContainer.AGGRESSIVE_REMOVAL_NAME,
    ctx.browserMetricsDAO.where(EQ(TYPE, Type.AGGRESSIVE_REMOVAL)));

// TODO(markdittmer): Explicitly select webSocketService port.
// (Current default: 4000.)
//
// Start service by accessing lazily constructed property.
ctx.webSocketService;
logger.log(`Serving web sockets from ${ctx.webSocketService.port}`);

server.start();
