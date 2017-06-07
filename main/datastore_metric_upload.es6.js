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
require('../lib/confluence/aggressive_removal.es6.js');
require('../lib/datastore/import_controller.es6.js');

const credentials = JSON.parse(fs.readFileSync(
    path.resolve(__dirname, '../.local/credentials.json')));

const logger = global.logger =
    foam.lookup('foam.nanos.log.ConsoleLogger').create();

const datastoreCtx = global.datastoreCtx =
    foam.lookup('org.chromium.apis.web.ImportController').create({
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
// TODO(markdittmer): Rename ImportController; it's a misnomer (more of a
// ContextContainer, or similar).
const localCtx = global.localCtx =
    foam.lookup('org.chromium.apis.web.ImportController').create({
      releaseDAO: MDAO.create({of: Release}),
      webInterfaceDAO: MDAO.create({of: WebInterface}),
      releaseWebInterfaceJunctionDAO: MDAO.create({
        of: ReleaseWebInterfaceJunction,
      }),
      authAgent: null,
      logger: logger,
    }).ctx;

const AnonymousSink = foam.lookup('foam.dao.AnonymousSink');
let promises = [];
function forwardDAOs(dao1, dao2) {
  return dao1.select(AnonymousSink.create({sink: {
    put: function(o) {
      logger.info('Storing ' + o.id);
      promises.push(dao2.put(o.cls_.create(o, localCtx)));
    },
  }}));
}

logger.info('Downloading API data snapshot');
Promise.all([
  forwardDAOs(datastoreCtx.releaseDAO, localCtx.releaseDAO),
  forwardDAOs(datastoreCtx.webInterfaceDAO, localCtx.webInterfaceDAO),
  forwardDAOs(datastoreCtx.releaseWebInterfaceJunctionDAO,
              localCtx.releaseWebInterfaceJunctionDAO),
]).then(function() {
  logger.info('Waiting for local store to settle');
  return Promise.all(promises);
}).then(function() {
  logger.info('Computing metrics');
  const aggressiveRemovalComputer =
      foam.lookup('org.chromium.apis.web.AggressiveRemoval')
      .create(null, localCtx);
  return aggressiveRemovalComputer.run();
});
