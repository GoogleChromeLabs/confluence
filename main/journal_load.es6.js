// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

const fs = require('fs');
const path = require('path');

global.FOAM_FLAGS = {gcloud: true};
require('foam2');

require('../lib/confluence/api_velocity_data.es6.js');
require('../lib/confluence/browser_metric_data.es6.js');
require('../lib/dao_container.es6.js');
require('../lib/web_apis/release.es6.js');
require('../lib/web_apis/release_interface_relationship.es6.js');
require('../lib/web_apis/web_interface.es6.js');
const pkg = org.chromium.apis.web;

// Need versioned classes. (Usually auto-instantiated by SyncDAO).
foam.version.VersionedClassFactorySingleton.create().get(pkg.Release);
foam.version.VersionedClassFactorySingleton.create().get(pkg.WebInterface);
foam.version.VersionedClassFactorySingleton.create()
    .get(pkg.ReleaseWebInterfaceJunction);
foam.version.VersionedClassFactorySingleton.create().get(pkg.BrowserMetricData);
foam.version.VersionedClassFactorySingleton.create().get(pkg.ApiVelocityData);

const logger = foam.log.ConsoleLogger.create();

const credentials = JSON.parse(fs.readFileSync(
    path.resolve(__dirname, '../.local/credentials.json')));

function getLocalDAO(name, cls, ctx) {
  return foam.dao.JDAO.create({
    delegate: foam.dao.MDAO.create({of: cls}, ctx),
    journal: foam.dao.NodeFileJournal.create({
      fd: fs.openSync(
          path.resolve(__dirname, `../data/${name}-journal.js`),
          // Read-only JDAO.
          'r'),
    }, ctx),
  }, ctx);
}

let localCtx = pkg.DAOContainer.create();
const releaseLocalDAO = localCtx.releaseDAO =
      getLocalDAO(pkg.Release.id, pkg.Release, localCtx);
const webInterfaceLocalDAO = localCtx.webInterfaceDAO =
      getLocalDAO(pkg.WebInterface.id, pkg.WebInterface, localCtx);
const releaseWebInterfaceJunctionLocalDAO =
      localCtx.releaseWebInterfaceJunctionDAO =
      getLocalDAO(pkg.ReleaseWebInterfaceJunction.id,
                  pkg.ReleaseWebInterfaceJunction,
                  localCtx);
const browserMetricsLocalDAO = localCtx.browserMetricsDAO =
      getLocalDAO(pkg.BrowserMetricData.id,
                  pkg.BrowserMetricData,
                  localCtx);
const apiVelocityLocalDAO = localCtx.apiVelocityDAO =
      getLocalDAO(pkg.ApiVelocityData.id, pkg.ApiVelocityData, localCtx);
localCtx.validate();

function jsonify(name, dao) {
  return dao.select().then(function(sink) {
    fs.writeFileSync(path.resolve(__dirname, `../data/${name}.json`),
                     foam.json.Storage.stringify(sink.array));
  });
}

logger.info('Loading data from journals');
Promise.all([
  releaseLocalDAO.synced,
  webInterfaceLocalDAO.synced,
  releaseWebInterfaceJunctionLocalDAO.synced,
  browserMetricsLocalDAO.synced,
  apiVelocityLocalDAO.synced,
]).then(function() {
  logger.info('Data loaded from journals');
  logger.info('JSONifying data');
  Promise.all([
    jsonify(pkg.Release.id, releaseLocalDAO),
    jsonify(pkg.WebInterface.id, webInterfaceLocalDAO),
    jsonify(pkg.ReleaseWebInterfaceJunction.id,
            releaseWebInterfaceJunctionLocalDAO),
    jsonify(pkg.BrowserMetricData.id, browserMetricsLocalDAO),
    jsonify(pkg.ApiVelocityData.id, apiVelocityLocalDAO),
  ]);
}).then(function() {
  logger.info('Data JSONified');
});
