// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

const fs = require('fs');
const path = require('path');
const process = require('process');

global.FOAM_FLAGS = {gcloud: true};
require('foam2');

require('../lib/confluence/aggressive_removal.es6.js');
require('../lib/confluence/api_velocity.es6.js');
require('../lib/confluence/browser_specific.es6.js');
require('../lib/confluence/failure_to_ship.es6.js');
require('../lib/json_dao_container.es6.js');
require('../lib/server/server.es6.js');
require('../lib/web_apis/release.es6.js');
require('../lib/web_apis/release_interface_relationship.es6.js');
require('../lib/web_apis/web_interface.es6.js');
const pkg = org.chromium.apis.web;

foam.CLASS({
  refines: 'foam.net.node.Handler',

  documentation: `Report raw message (no potentially identifying metadata) in
      request handlers`,

  methods: [
    function reportWarnMsg(req, msg) { this.warn(msg); },
    function reportErrorMsg(req, msg) { this.error(msg); },
  ],
});

let server = pkg.Server.create({
  port: 8080,
});

const logger = foam.log.ConsoleLogger.create();
const modeString = process.argv[2];
const mode = pkg.JsonDAOContainerMode.VALUES.filter(function(value) {
  return value.name === modeString;
})[0] || pkg.JsonDAOContainerMode.LOCAL;
const basename = mode === pkg.JsonDAOContainerMode.LOCAL ?
      `${__basename}/../data/json` :
      'https://storage.googleapis.com/web-api-confluence-data-cache/latest/json';
const daoContainer = pkg.JsonDAOContainer.create({
  config: pkg.JsonDAOContainerConfig.create({
    mode: mode,
    basename: basename,
  }),
});
const ctx = daoContainer.ctx;

function registerDAO(name, dao) {
  foam.assert(dao, 'Broken use of helper: registerDAO()');
  const url = `/${name}`;
  logger.info(`Exporting REST DAO endpoint: ${url}`);
  server.exportDAO(dao, url);
}

registerDAO(daoContainer.RELEASE_NAME, ctx.releaseDAO);
registerDAO(daoContainer.WEB_INTERFACE_NAME, ctx.webInterfaceDAO);
registerDAO(daoContainer.RELEASE_WEB_INTERFACE_JUNCTION_NAME,
            ctx.releaseWebInterfaceJunctionDAO);
registerDAO(daoContainer.API_VELOCITY_NAME, ctx.apiVelocityDAO);

const E = foam.mlang.ExpressionsSingleton.create();
const EQ = E.EQ.bind(E);
const Type = pkg.BrowserMetricDataType;
const TYPE = pkg.BrowserMetricData.TYPE;

registerDAO(
    daoContainer.FAILURE_TO_SHIP_NAME,
    ctx.browserMetricsDAO.where(EQ(TYPE, Type.FAILURE_TO_SHIP)));
registerDAO(
    daoContainer.BROWSER_SPECIFIC_NAME,
    ctx.browserMetricsDAO.where(EQ(TYPE, Type.BROWSER_SPECIFIC)));
registerDAO(
    daoContainer.AGGRESSIVE_REMOVAL_NAME,
    ctx.browserMetricsDAO.where(EQ(TYPE, Type.AGGRESSIVE_REMOVAL)));

server.exportFile('/', path.resolve(__dirname, '../static/index.html'));
server.exportDirectory('/images', path.resolve(__dirname, '../static/images'));
server.exportDirectory('/', path.resolve(__dirname, '../static/bundle'));

server.start();
