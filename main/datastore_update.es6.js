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

require('../lib/confluence/set_ops.es6.js');
require('../lib/datastore/datastore_container.es6.js');
require('../lib/web_catalog/object_graph_importer.es6.js');
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
const localCtx = foam.__context__.createSubContext({
  releaseDAO: MDAO.create({of: Release}),
  webInterfaceDAO: MDAO.create({of: WebInterface}),
  releaseWebInterfaceJunctionDAO: MDAO.create({
    of: ReleaseWebInterfaceJunction,
  }),
  logger: logger,
});
const localImporter = global.localImporter =
    foam.lookup('org.chromium.apis.web.ObjectGraphImporter').create({
      objectGraphPath: path.resolve(__dirname, '../data/og'),
    }, localCtx);

const E = foam.lookup('foam.mlang.ExpressionsSingleton').create();

const putMissing = global.putMissing = function(local, remote) {
  return local.select(
      E.SET_MINUS(remote, foam.lookup('foam.dao.AnonymousSink').create({
        sink: {
          put: function(o) {
            logger.info('Putting missing (' + local.of.id + '): ' + o.id);
            remote.put(o);
          },
        },
      })));
};

const verify = global.verify = function(local, remote) {
  return Promise.all([local, remote]).then(function(sinks) {
    if (sinks[0].value !== sinks[1].value) {
      logger.error('Counts are different (' + local.of.id + '): ' +
          sinks[0].value + ', ' + sinks[1].value);
      return putMissing(local, remote).then(() => sinks);
    } else {
      logger.info('Counts match (' + local.of.id + ')');
      return sinks;
    }
  });
};

const imported = global.imported = localImporter.import();
global.releaseCounts =
    imported.then(() => verify(localCtx.releaseDAO, datastoreCtx.releaseDAO));
global.apiCounts = imported.then(() => verify(localCtx.webInterfaceDAO,
                                              datastoreCtx.webInterfaceDAO));
global.releaseAPICounts =
    imported.then(() => verify(localCtx.releaseWebInterfaceJunctionDAO,
                               datastoreCtx.releaseWebInterfaceJunctionDAO));
