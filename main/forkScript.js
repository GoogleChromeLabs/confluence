// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

//
// Similar to foam2/src/foam/box/node/forkScript.js, but require() application
// code and perform code generation steps before connecting to parent.
//

const path = require('path');
const process = require('process');

global.FOAM_FLAGS = {gcloud: true};
require(path.resolve(`${__dirname}/../node_modules/foam2/src/foam.js`));

require('../lib/compat.es6.js');
require('../lib/confluence/api_count.es6.js');
require('../lib/confluence/browser_specific.es6.js');
require('../lib/confluence/lone_omission.es6.js');
require('../lib/confluence/lone_removal.es6.js');
require('../lib/confluence/metric_computer_service.es6.js');
require('../lib/dao/dao_container.es6.js');
require('../lib/dao/http_json_dao.es6.js');
require('../lib/dao/local_json_dao.es6.js');
require('../lib/data_source.es6.js');
require('../lib/parse/expressions.es6.js');
require('../lib/web_apis/api_compat_data.es6.js');
require('../lib/web_apis/release.es6.js');
require('../lib/web_apis/release_interface_relationship.es6.js');
require('../lib/web_apis/web_interface.es6.js');
require('../lib/web_catalog/api_extractor_service.es6.js');
const pkg = org.chromium.apis.web;

const USAGE = `USAGE:

    node /path/to/forkScript.js DataSource

        DataSource = [ ${pkg.DataSource.VALUES
      .map((value) => value.name).join(' | ')} ]`;

const logger = foam.log.ConsoleLogger.create();

const compatClassFile = pkg.DAOContainer.COMPAT_MODEL_FILE_NAME;

function panic(msg) {
  console.error(msg);
  console.error(USAGE);
  process.exit(1);
}

function getMode(str) {
  const mode = pkg.DataSource.VALUES.find(function(value) {
    return value.name === str;
  });

  if (mode) return mode;

  const modeNames = pkg.DataSource.VALUES.map(function(value) {
    return value.name;
  });
  panic(`Invalid ${pkg.DataSource.name} (not one of "${modeNames
      .join('", "')}")`);

  return null;
}

// process.argv = /path/to/node /path/to/script <args>
const args = process.argv.slice(2);
if (args.length !== 1) panic('Expected exactly one script argument');
const mode = getMode(args[0]);

const start = () => {
  const container = pkg.DAOContainer.create(null, logger);
  foam.box.node.ForkBox.CONNECT_TO_PARENT(
      foam.box.Context.create({
        unsafe: false,
        classWhitelist: require('../data/class_whitelist.json'),
      }, container));
};

// TODO(markdittmer): This should be local or remote based on param passed to
// parent. It should be forwarded to forkScript invocation.
const compatClassURL = mode === pkg.DataSource.LOCAL ?
    `file://${__dirname}/../data/json/${compatClassFile}` :
    `${require('../data/http_json_dao_base_url.json')}/${compatClassFile}`;
org.chromium.apis.web.ClassGenerator.create({
  classURL: compatClassURL,
}).generateClass().then(start, (err) => {
  logger.warn(err);
  logger.warn(`Fork (PID=${process.pid}) encountered a dynamic class loading error; continuing anyway...`);
  start();
});
