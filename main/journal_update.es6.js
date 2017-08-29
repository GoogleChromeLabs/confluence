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
require('../lib/datastore/datastore_container.es6.js');
require('../lib/web_apis/release.es6.js');
require('../lib/web_apis/release_interface_relationship.es6.js');
require('../lib/web_apis/web_interface.es6.js');
const pkg = org.chromium.apis.web;

const logger = foam.log.ConsoleLogger.create();

const credentials = JSON.parse(fs.readFileSync(
    path.resolve(__dirname, '../.local/credentials.json')));

const datastoreCtx = pkg.DatastoreContainer.create({
  mode: pkg.DatastoreContainerMode.DATA_COLLECTOR,
  gcloudAuthEmail: credentials.client_email,
  gcloudAuthPrivateKey: credentials.private_key,
  gcloudProjectId: credentials.project_id,
  logger: logger,
}).ctx;

function getLocalDAO(name, cls, ctx) {
  return foam.dao.JDAO.create({
    of: cls,
    delegate: foam.dao.NullDAO.create({of: cls}, ctx),
    journal: foam.dao.NodeFileJournal.create({
      of: cls,
      fd: fs.openSync(
          path.resolve(__dirname, `../data/${name}-journal.js`),
          // Truncate journal.
          'w+'),
    }, ctx),
  }, ctx);
}

let releaseSyncDAO = datastoreCtx.releaseDAO;
let webInterfaceSyncDAO = datastoreCtx.webInterfaceDAO;
let releaseWebInterfaceJunctionSyncDAO =
    datastoreCtx.releaseWebInterfaceJunctionDAO;
let browserMetricsSyncDAO = datastoreCtx.browserMetricsDAO;
let apiVelocitySyncDAO = datastoreCtx.apiVelocityDAO;

// Overwrite delegates with local journaling DAO.
releaseSyncDAO.delegate = getLocalDAO(
    pkg.VersionedRelease.id,
    pkg.VersionedRelease,
    datastoreCtx);
webInterfaceSyncDAO.delegate = getLocalDAO(
    pkg.VersionedWebInterface.id,
    pkg.VersionedWebInterface,
    datastoreCtx);
releaseWebInterfaceJunctionSyncDAO.delegate = getLocalDAO(
    pkg.VersionedReleaseWebInterfaceJunction.id,
    pkg.VersionedReleaseWebInterfaceJunction,
    datastoreCtx);
browserMetricsSyncDAO.delegate = getLocalDAO(
    pkg.VersionedBrowserMetricData.id,
    pkg.VersionedBrowserMetricData,
    datastoreCtx);
apiVelocitySyncDAO.delegate = getLocalDAO(
    pkg.VersionedApiVelocityData.id,
    pkg.VersionedApiVelocityData,
    datastoreCtx);

logger.info('Syncing from datastore');
Promise.all([
  // Wait for SyncDAOs to be synced.
  releaseSyncDAO.synced,
  webInterfaceSyncDAO.synced,
  releaseWebInterfaceJunctionSyncDAO.synced,
  browserMetricsSyncDAO.synced,
  apiVelocitySyncDAO.synced,
]).then(function() {
  logger.info('Synced from datastore');
  logger.info('Journaling data');
  return Promise.all([
    // Wait for journaling DAOs to be synced.
    releaseSyncDAO.delegate.synced,
    webInterfaceSyncDAO.delegate.synced,
    releaseWebInterfaceJunctionSyncDAO.delegate.synced,
    browserMetricsSyncDAO.delegate.synced,
    apiVelocitySyncDAO.delegate.synced,
  ]);
}).then(function() {
  logger.info('Journaling complete');
});
