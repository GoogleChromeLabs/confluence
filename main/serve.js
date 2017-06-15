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
  port: 9010,
});

const credentials = JSON.parse(fs.readFileSync(
    path.resolve(__dirname, '../.local/credentials.json')));
const logger = foam.lookup('foam.nanos.log.ConsoleLogger').create();
const ctx = foam.lookup('org.chromium.apis.web.DatastoreContainer').create({
  gcloudAuthEmail: credentials.client_email,
  gcloudAuthPrivateKey: credentials.private_key,
  gcloudProjectId: credentials.project_id,
  logger: logger,
}).ctx;

server.exportFile('/', `${__dirname}/../static/index.html`);

server.exportDAO(ctx.releaseDAO, '/releases');
server.exportDAO(ctx.webInterfaceDAO, '/web-interfaces');
server.exportDAO(ctx.releaseWebInterfaceJunctionDAO, '/release-apis');

server.exportDAO(ctx.apiVelocityDAO, '/api-velocity');

const E = foam.lookup('foam.mlang.ExpressionsSingleton').create();
const EQ = E.EQ.bind(E);
const Type = foam.lookup('org.chromium.apis.web.BrowserMetricDataType');
const TYPE = foam.lookup('org.chromium.apis.web.BrowserMetricData').TYPE;

server.exportDAO(ctx.browserMetricsDAO.where(EQ(TYPE, Type.FAILURE_TO_SHIP)),
                 '/failure-to-ship');
server.exportDAO(ctx.browserMetricsDAO.where(EQ(TYPE, Type.BROWSER_SPECIFIC)),
                 '/browser-specific');
server.exportDAO(ctx.browserMetricsDAO.where(EQ(TYPE, Type.AGGRESSIVE_REMOVAL)),
                 '/aggressive-removal');

server.exportDirectory('/images', `${__dirname}/../static/images`);

server.exportDirectory('/', `${__dirname}/../static/bundle`);

server.start();
