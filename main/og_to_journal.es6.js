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
require('../lib/confluence/api_velocity_data.es6.js');
require('../lib/confluence/browser_metric_data.es6.js');
require('../lib/confluence/browser_specific.es6.js');
require('../lib/confluence/failure_to_ship.es6.js');
require('../lib/confluence/set_ops.es6.js');
require('../lib/dao_container.es6.js');
require('../lib/datastore/datastore_container.es6.js');
require('../lib/web_apis/release.es6.js');
require('../lib/web_apis/release_interface_relationship.es6.js');
require('../lib/web_apis/web_interface.es6.js');
require('../lib/web_catalog/object_graph_importer.es6.js');
const pkg = org.chromium.apis.web;

const logger = foam.log.ConsoleLogger.create();

function getJournalDAO(name, cls, ctx, mode) {
  const filename = path.resolve(__dirname, `../data/${name}-journal.js`);
  logger.info(`Creating JDAO (mode=${mode}) in ${filename}`);
  return foam.dao.JDAO.create({
    of: cls,
    delegate: foam.dao.MDAO.create({of: cls}, ctx),
    journal: foam.dao.NodeFileJournal.create({
      of: cls,
      fd: fs.openSync(
          filename,
          mode),
    }, ctx),
  }, ctx);
}
function getOverwriteJournalDAO(name, cls, ctx) {
  return getJournalDAO(name, cls, ctx, 'w+');
}
function getReadJournalDAO(name, cls, ctx) {
  return getJournalDAO(name, cls, ctx, 'r');
}

const ctx = pkg.DAOContainer.create(null, foam.__context__.createSubContext({
  logger: logger,
}));

const importer = pkg.ObjectGraphImporter.create({
  objectGraphPath: path.resolve(__dirname, '../data/og'),
}, ctx);

ctx.releaseDAO = getOverwriteJournalDAO(pkg.Release.id, pkg.Release, ctx);
ctx.webInterfaceDAO = getOverwriteJournalDAO(
    pkg.WebInterface.id, pkg.WebInterface, ctx);
ctx.releaseWebInterfaceJunctionDAO = getOverwriteJournalDAO(
    pkg.ReleaseWebInterfaceJunction.id,
    pkg.ReleaseWebInterfaceJunction,
    ctx);
ctx.apiVelocityDAO = getOverwriteJournalDAO(
    pkg.ApiVelocityData.id, pkg.ApiVelocityData, ctx);
ctx.browserMetricsDAO = getOverwriteJournalDAO(
    pkg.BrowserMetricData.id, pkg.BrowserMetricData, ctx);

logger.info('Adding junction DAO indices');
const junctionDAO = ctx.releaseWebInterfaceJunctionDAO.delegate;
junctionDAO.addPropertyIndex(
    pkg.ReleaseWebInterfaceJunction.SOURCE_ID);
junctionDAO.addPropertyIndex(
    pkg.ReleaseWebInterfaceJunction.TARGET_ID);
junctionDAO.addPropertyIndex(
    pkg.ReleaseWebInterfaceJunction.SOURCE_ID,
    pkg.ReleaseWebInterfaceJunction.TARGET_ID);
logger.info('Added junction DAO indices');

logger.info('Importing API data');
importer.import().then(function() {
  logger.info('Waiting for API data journals to settle');
  return Promise.all([
    ctx.releaseDAO.synced,
    ctx.webInterfaceDAO.synced,
    ctx.releaseWebInterfaceJunctionDAO.synced,
  ]);
}).then(function() {
  logger.info('API data imported');

  logger.info('Computing API metrics');
  return Promise.all([
    pkg.AggressiveRemoval.create(null, ctx).run(),
    pkg.BrowserSpecific.create(null, ctx).run(),
    pkg.FailureToShip.create(null, ctx).run(),
    pkg.ApiVelocity.create(null, ctx).run(),
  ]);
}).then(function() {
  logger.info('Waiting for API metrics journals to settle');
  return Promise.all([
    ctx.browserMetricsDAO.synced,
    ctx.apiVelocityDAO.synced,
  ]);
}).then(function() {
    logger.info('Computed API metrics');
});
