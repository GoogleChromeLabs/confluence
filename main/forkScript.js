// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

//
// Similar to foam2/src/foam/box/node/forkScript.js, but require() application
// code and perform code generation steps before connecting to parent.
//

const fs = require('fs');
const path = require('path');

global.FOAM_FLAGS = {gcloud: true};
require(path.resolve(`${__dirname}/../node_modules/foam2/src/foam.js`));

require('../lib/compat.es6.js');
require('../lib/confluence/lone_removal.es6.js');
require('../lib/confluence/api_count.es6.js');
require('../lib/confluence/browser_specific.es6.js');
require('../lib/confluence/lone_omission.es6.js');
require('../lib/confluence/metric_computer_service.es6.js');
require('../lib/dao/dao_container.es6.js');
require('../lib/dao/http_json_dao.es6.js');
require('../lib/dao/local_json_dao.es6.js');
require('../lib/parse/expressions.es6.js');
require('../lib/web_apis/api_compat_data.es6.js');
require('../lib/web_apis/release_interface_relationship.es6.js');
require('../lib/web_apis/release.es6.js');
require('../lib/web_apis/web_interface.es6.js');
require('../lib/web_catalog/api_extractor_service.es6.js');
const pkg = org.chromium.apis.web;

const logger = foam.log.ConsoleLogger.create();

const compatClassFile = pkg.DAOContainer.COMPAT_MODEL_FILE_NAME;

// TODO(markdittmer): This should be local or remote based on param passed to
// parent. It should be forwarded to forkScript invocation.
const compatClassURL =
    `${require('../data/http_json_dao_base_url.json')}/${compatClassFile}`;
org.chromium.apis.web.ClassGenerator.create({
  classURL: compatClassURL,
}).generateClass().then(() => {
  const container = pkg.DAOContainer.create(null, logger);
  foam.box.node.ForkBox.CONNECT_TO_PARENT(
      foam.box.Context.create({
        unsafe: false,
        classWhitelist: require('../data/class_whitelist.json'),
      }, container));
}, err => {
  logger.error(err);
  process.exit(1);
});
