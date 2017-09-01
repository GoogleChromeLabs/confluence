// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

console.log(`Fork spawned ${require('process').pid}`);

//
// Same as foam2/src/foam/box/node/forkScript.js, but require() application
// code.
//

const fs = require('fs');
const path = require('path');

global.FOAM_FLAGS = {gcloud: true};
require(path.resolve(`${__dirname}/../node_modules/foam2/src/foam.js`));

require('../lib/confluence/aggressive_removal.es6.js');
require('../lib/confluence/api_velocity.es6.js');
require('../lib/confluence/browser_specific.es6.js');
require('../lib/confluence/failure_to_ship.es6.js');
require('../lib/datastore/datastore_container.es6.js');
require('../lib/sync_dao.es6.js');
require('../lib/web_apis/release.es6.js');
require('../lib/web_apis/release_interface_relationship.es6.js');
require('../lib/web_apis/web_interface.es6.js');
const pkg = org.chromium.apis.web;

// Setup BaseDatastoreContainer for logging and authenticated Datastore access.
const credentials = JSON.parse(fs.readFileSync(
    path.resolve(__dirname, '../.local/credentials.json')));
const logger = foam.log.ConsoleLogger.create();
const daoContainer = pkg.BaseDatastoreContainer.create({
  gcloudAuthEmail: credentials.client_email,
  gcloudAuthPrivateKey: credentials.private_key,
  gcloudProjectId: credentials.project_id,
  logger: logger,
});
const ctx = daoContainer.ctx;

ctx.socketService.listening$.sub(function(sub, _, __, slot) {
  if (!slot.get()) return;

  sub.detach();
  var stdin = require('process').stdin;
  var buf = '';
  stdin.on('data', function(data) {
    buf += data.toString();
  });
  stdin.on('end', function() {
    // TODO(markdittmer): Use secure parser.
    foam.json.parseString(buf, ctx).send(foam.box.Message.create({
      // TODO(markdittmer): RegisterSelfMessage should handle naming. Is "name:"
      // below necessary?
      object: foam.box.SocketBox.create({
        name: ctx.me.name,
        address: `0.0.0.0:${ctx.socketService.port}`
      })
    }));
  });
});
