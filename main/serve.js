// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

//
// Run web service.
//
// See USAGE string for usage details.
//

const fs = require('fs');
const path = require('path');
const process = require('process');

global.FOAM_FLAGS = {gcloud: true};
require('foam2');

require('../lib/compat.es6.js');
require('../lib/confluence/aggressive_removal.es6.js');
require('../lib/confluence/api_velocity.es6.js');
require('../lib/confluence/browser_specific.es6.js');
require('../lib/confluence/failure_to_ship.es6.js');
require('../lib/dao/json_dao_container.es6.js');
require('../lib/server/server.es6.js');
require('../lib/web_apis/api_compat_data.es6.js');
require('../lib/web_apis/release.es6.js');
require('../lib/web_apis/release_interface_relationship.es6.js');
require('../lib/web_apis/web_interface.es6.js');
const pkg = org.chromium.apis.web;

const USAGE = `USAGE:

    node /path/to/serve.js JsonDAOContainerMode ServerMode

        JsonDAOContainerMode = [ ${pkg.JsonDAOContainerMode.VALUES.map(
                                       value => value.name).join(' | ')} ]

        ServerMode = [ ${pkg.ServerMode.VALUES.map(
                             value => value.name).join(' | ')} ]`;

if (process.argv.length !== 4) {
  console.error(USAGE);
  process.exit(1);
}

foam.CLASS({
  refines: 'foam.net.node.Handler',

  documentation: `Report raw message (no potentially identifying metadata) in
      request handlers`,

  methods: [
    function reportWarnMsg(req, msg) { this.warn(msg); },
    function reportErrorMsg(req, msg) { this.error(msg); },
  ],
});

function getModeString(Enum, str) {
  const mode = Enum.VALUES.filter(function(value) {
    return value.name === str;
  })[0];

  if (mode) return mode;

  const modeNames = Enum.VALUES.map(function(value) {
    return value.name;
  });
  console.error(`Invalid ${Enum.name}`);
  console.error(USAGE);

  process.exit(1);
  return null;
}

const containerMode = getModeString(pkg.JsonDAOContainerMode, process.argv[2]);
const serverMode = getModeString(pkg.ServerMode, process.argv[3]);


const logger = foam.log.ConsoleLogger.create();

const basename = containerMode === pkg.JsonDAOContainerMode.LOCAL ?
      `file://${__dirname}/../data/json` :
      require('../data/http_json_dao_base_url.json');

const compatClassFile = 'class:org.chromium.apis.web.generated.CompatData.json';
const compatClassURL = `${basename}/${compatClassFile}`;
org.chromium.apis.web.ClassGenerator.create({
  classURL: compatClassURL,
}).generateClass().then(() => {
  const daoContainer = pkg.JsonDAOContainer.create({
    mode: containerMode,
    basename: basename,
  });
  const ctx = daoContainer.ctx;
  pkg.Server.create({
    mode: serverMode,
    port: 8080,
  }, ctx).start();
}, err => {
  logger.error(err);
  process.exit(1)
});
