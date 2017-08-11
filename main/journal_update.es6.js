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
    delegate: foam.dao.NullDAO.create({of: cls}, ctx),
    journal: foam.dao.NodeFileJournal.create({
      fd: fs.openSync(
          path.resolve(__dirname, `../data/${name}-journal.js`),
          // Do not truncate journal.
          'a+'),
    }, ctx),
  }, ctx);
}

// let localCtx = pkg.DAOContainer.create();
// const releaseLocalDAO = localCtx.releaseDAO = getLocalDAO(pkg.Release);
// const webInterfaceLocalDAO = localCtx.webInterfaceDAO =
//       getLocalDAO(pkg.WebInterface.id, pkg.WebInterface, localCtx);
// const releaseWebInterfaceJunctionLocalDAO =
//       localCtx.releaseWebInterfaceJunctionDAO =
//       getLocalDAO(pkg.ReleaseWebInterfaceJunction.id,
//                   pkg.ReleaseWebInterfaceJunction,
//                   localCtx);
// const browserMetricsLocalDAO = localCtx.browserMetricsDAO =
//       getLocalDAO(pkg.BrowserMetricData.id,
//                   pkg.BrowserMetricData,
//                   localCtx);
// const apiVelocityLocalDAO = localCtx.apiVelocityDAO =
//       getLocalDAO(pkg.ApiVelocityData.id, pkg.ApiVelocityData, localCtx);
// localCtx.validate();

let releaseSyncDAO = datastoreCtx.releaseDAO;
let webInterfaceSyncDAO = datastoreCtx.webInterfaceDAO;
let releaseWebInterfaceJunctionSyncDAO =
    datastoreCtx.releaseWebInterfaceJunctionDAO;
let browserMetricsSyncDAO = datastoreCtx.browserMetricsDAO;
let apiVelocitySyncDAO = datastoreCtx.apiVelocityDAO;

releaseSyncDAO.syncRecordDAO = getLocalDAO(
    `foam.dao.sync.VersionedSyncRecord-${pkg.Release.id}`,
    pkg.Release,
    datastoreCtx);
releaseSyncDAO.delegate = getLocalDAO(
    pkg.Release.id,
    pkg.Release,
    datastoreCtx);
webInterfaceSyncDAO.syncRecordDAO = getLocalDAO(
    `foam.dao.sync.VersionedSyncRecord-${pkg.WebInterface.id}`,
    pkg.WebInterface,
    datastoreCtx);
webInterfaceSyncDAO.delegate = getLocalDAO(
    pkg.WebInterface.id,
    pkg.WebInterface,
    datastoreCtx);
releaseWebInterfaceJunctionSyncDAO.syncRecordDAO = getLocalDAO(
    `foam.dao.sync.VersionedSyncRecord-${pkg.ReleaseWebInterfaceJunction.id}`,
    pkg.ReleaseWebInterfaceJunction,
    datastoreCtx);
releaseWebInterfaceJunctionSyncDAO.delegate = getLocalDAO(
    pkg.ReleaseWebInterfaceJunction.id,
    pkg.ReleaseWebInterfaceJunction,
    datastoreCtx);
browserMetricsSyncDAO.syncRecordDAO = getLocalDAO(
    `foam.dao.sync.VersionedSyncRecord-${pkg.BrowserMetricData.id}`,
    pkg.BrowserMetricData,
    datastoreCtx);
browserMetricsSyncDAO.delegate = getLocalDAO(
    pkg.BrowserMetricData.id,
    pkg.BrowserMetricData,
    datastoreCtx);
apiVelocitySyncDAO.syncRecordDAO = getLocalDAO(
    `foam.dao.sync.VersionedSyncRecord-${pkg.ApiVelocityData.id}`,
    pkg.ApiVelocityData,
    datastoreCtx);
apiVelocitySyncDAO.delegate = getLocalDAO(
    pkg.ApiVelocityData.id,
    pkg.ApiVelocityData,
    datastoreCtx);

logger.info('Syncing from datastore');
Promise.all([
  releaseSyncDAO.synced,
  webInterfaceSyncDAO.synced,
  releaseWebInterfaceJunctionSyncDAO.synced,
  browserMetricsSyncDAO.synced,
  apiVelocitySyncDAO.synced,
]).then(function() {
  logger.info('Synced from datastore');
  logger.info('Journaling data');
  return Promise.all([
    // Finish journaling sync records.
    releaseSyncDAO.syncRecordDAO.synced,
    webInterfaceSyncDAO.syncRecordDAO.synced,
    releaseWebInterfaceJunctionSyncDAO.syncRecordDAO.synced,
    browserMetricsSyncDAO.syncRecordDAO.synced,
    apiVelocitySyncDAO.syncRecordDAO.synced,
    // Finish journaling data.
    releaseSyncDAO.delegate.synced,
    webInterfaceSyncDAO.delegate.synced,
    releaseWebInterfaceJunctionSyncDAO.delegate.synced,
    browserMetricsSyncDAO.delegate.synced,
    apiVelocitySyncDAO.delegate.synced,
  ]);
}).then(function() {
  logger.info('Journaling complete');
});
