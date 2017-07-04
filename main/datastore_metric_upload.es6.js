// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

//
// Compare local API data based on object graph JSON blobs to remote (Cloud
// Datastore) data. Use count-of-entities as a proxy for "needs sync". Use
// SET_MINUS to filter out existing data and upload missing data.
//

const fs = require('fs');
const path = require('path');

global.FOAM_FLAGS = {gcloud: true};
require('foam2');
require('../lib/datastore/datastore_container.es6.js');
require('../lib/web_apis/release.es6.js');
require('../lib/web_apis/release_interface_relationship.es6.js');
require('../lib/web_apis/web_interface.es6.js');

const credentials = JSON.parse(fs.readFileSync(
    path.resolve(__dirname, '../.local/credentials.json')));

const logger = global.logger =
    foam.lookup('foam.log.ConsoleLogger').create();

const datastoreCtx = global.datastoreCtx =
    foam.lookup('org.chromium.apis.web.DatastoreContainer').create({
      gcloudAuthEmail: credentials.client_email,
      gcloudAuthPrivateKey: credentials.private_key,
      gcloudProjectId: credentials.project_id,
      logger: logger,
    }).ctx;

const MDAO = foam.lookup('foam.dao.MDAO');
const Release = foam.lookup('org.chromium.apis.web.Release');
const WebInterface = foam.lookup('org.chromium.apis.web.WebInterface');
const ReleaseWebInterfaceJunction =
    foam.lookup('org.chromium.apis.web.ReleaseWebInterfaceJunction');
// Create context for computing metrics against local data:
// - Local release, API, and relase/API DAOs for computing metrics,
// - Remote browser metrics DAO pushing metrics to Datastore.
//
// This way the upload back to datastore happens automatically when metrics are
// produced by the underlying metric computer classes.
const computeMetricsCtx = global.computeMetricsCtx =
    foam.lookup('org.chromium.apis.web.DatastoreContainer').create({
      releaseDAO: MDAO.create({of: Release}),
      webInterfaceDAO: MDAO.create({of: WebInterface}),
      releaseWebInterfaceJunctionDAO: MDAO.create({
        of: ReleaseWebInterfaceJunction,
      }),
      // browserMetricsDAO: <default: Send to Datastore>,
      gcloudAuthEmail: credentials.client_email,
      gcloudAuthPrivateKey: credentials.private_key,
      gcloudProjectId: credentials.project_id,
      logger: logger,
    }).ctx;

// Add indexes to make relational lookups faster.
computeMetricsCtx.releaseWebInterfaceJunctionDAO.addPropertyIndex(
    ReleaseWebInterfaceJunction.SOURCE_ID);
computeMetricsCtx.releaseWebInterfaceJunctionDAO.addPropertyIndex(
    ReleaseWebInterfaceJunction.TARGET_ID);
computeMetricsCtx.releaseWebInterfaceJunctionDAO.addPropertyIndex(
    ReleaseWebInterfaceJunction.SOURCE_ID,
    ReleaseWebInterfaceJunction.TARGET_ID);

const AnonymousSink = foam.lookup('foam.dao.AnonymousSink');
let promises = [];
let computingMetrics = global.computingMetrics = false;
function forwardDAOs(src, dst) {
  var count = 0;
  var report = foam.__context__.merged(function report() {
    logger.info(count + ' ' + src.of.id + 's stored');
  }, 5000);
  return src.select(AnonymousSink.create({sink: {
    put: function(o) {
      foam.assert(
        !computingMetrics,
        'Data arrival after metric computation started');
      count++;
      report();
      promises.push(dst.put(o.cls_.create(o, computeMetricsCtx)));
    },
  }}));
}

logger.info('Downloading API data snapshot');
Promise.all([
  forwardDAOs(datastoreCtx.releaseDAO, computeMetricsCtx.releaseDAO),
  forwardDAOs(datastoreCtx.webInterfaceDAO, computeMetricsCtx.webInterfaceDAO),
  forwardDAOs(datastoreCtx.releaseWebInterfaceJunctionDAO,
              computeMetricsCtx.releaseWebInterfaceJunctionDAO),
]).then(function() {
  logger.info('Waiting for local store to settle');
  return Promise.all(promises);
}).then(function() {
  computingMetrics = global.computingMetrics = true;
  logger.info('Local store ready to go in context: global.computeMetricsCtx');

  // Support running interactively; only compute metrics immediately when this
  // module is "main".
  if (require.main === module) {
    require('../lib/confluence/aggressive_removal.es6.js');
    const aggressiveRemovalComputer =
        foam.lookup('org.chromium.apis.web.AggressiveRemoval')
        .create(null, computeMetricsCtx);
    require('../lib/confluence/browser_specific.es6.js');
    const browserSpecificComputer =
        foam.lookup('org.chromium.apis.web.BrowserSpecific')
        .create(null, computeMetricsCtx);
    require('../lib/confluence/failure_to_ship.es6.js');
    const failureToShipComputer =
        foam.lookup('org.chromium.apis.web.FailureToShip')
        .create(null, computeMetricsCtx);
    require('../lib/confluence/api_velocity.es6.js');
    const apiVelocityComputer =
        foam.lookup('org.chromium.apis.web.ApiVelocity')
        .create(null, computeMetricsCtx);

    return Promise.all([
      browserSpecificComputer.run(),
      failureToShipComputer.run(),
      aggressiveRemovalComputer.run(),
      apiVelocityComputer.run(),
    ]);
  } else {
    return computeMetricsCtx;
  }
});
