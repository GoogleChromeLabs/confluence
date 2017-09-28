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

const logger = foam.log.ConsoleLogger.create();

const credentials = JSON.parse(fs.readFileSync(
    path.resolve(__dirname, '../.local/credentials.json')));

function getLocalDAO(name, cls, ctx) {
  return foam.dao.JDAO.create({
    of: cls,
    delegate: foam.dao.MDAO.create({of: cls}, ctx),
    journal: foam.dao.NodeFileJournal.create({
      of: cls,
      fd: fs.openSync(
          path.resolve(__dirname, `../data/journal/${name}-journal.js`),
          // Read-only JDAO.
          'r'),
    }, ctx),
  }, ctx);
}

let localCtx = pkg.DAOContainer.create();
const releaseLocalDAO = localCtx.releaseDAO =
      getLocalDAO(pkg.VersionedRelease.id, pkg.VersionedRelease, localCtx);
const webInterfaceLocalDAO = localCtx.webInterfaceDAO =
      getLocalDAO(pkg.VersionedWebInterface.id, pkg.VersionedWebInterface, localCtx);
const releaseWebInterfaceJunctionLocalDAO =
      localCtx.releaseWebInterfaceJunctionDAO =
      getLocalDAO(pkg.VersionedReleaseWebInterfaceJunction.id,
                  pkg.VersionedReleaseWebInterfaceJunction,
                  localCtx);
const browserMetricsLocalDAO = localCtx.browserMetricsDAO =
      getLocalDAO(pkg.VersionedBrowserMetricData.id,
                  pkg.VersionedBrowserMetricData,
                  localCtx);
const apiVelocityLocalDAO = localCtx.apiVelocityDAO =
      getLocalDAO(pkg.VersionedApiVelocityData.id, pkg.VersionedApiVelocityData, localCtx);
localCtx.validate();

// Like foam.json.Storage, but strict.
var outputter = foam.json.Outputter.create({
  pretty: false,
  formatDatesAsNumbers: true,
  outputDefaultValues: false,
  strict: true,
  propertyPredicate: function(o, p) { return !p.storageTransient; }
});
function jsonify(name, dao) {
  return dao.select().then(function(sink) {
    fs.writeFileSync(path.resolve(__dirname, `../data/${name}.json`),
                     outputter.stringify(sink.array, dao.of));
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
    jsonify(pkg.VersionedRelease.id, releaseLocalDAO),
    jsonify(pkg.VersionedWebInterface.id, webInterfaceLocalDAO),
    jsonify(pkg.VersionedReleaseWebInterfaceJunction.id,
            releaseWebInterfaceJunctionLocalDAO),
    jsonify(pkg.VersionedBrowserMetricData.id, browserMetricsLocalDAO),
    jsonify(pkg.VersionedApiVelocityData.id, apiVelocityLocalDAO),
  ]);
}).then(function() {
  logger.info('Data JSONified');
});
