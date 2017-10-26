// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

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
require('../lib/confluence/metric_computer_service.es6.js');
require('../lib/dao_container.es6.js');
require('../lib/http_json_dao.es6.js');
require('../lib/local_json_dao.es6.js');
require('../lib/web_apis/release.es6.js');
require('../lib/web_apis/release_interface_relationship.es6.js');
require('../lib/web_apis/web_interface.es6.js');
require('../lib/web_catalog/api_extractor_service.es6.js');
const pkg = org.chromium.apis.web;

const logger = foam.log.ConsoleLogger.create();
const container = pkg.DAOContainer.create(null, logger);
foam.box.node.ForkBox.CONNECT_TO_PARENT(foam.box.Context.create({
  unsafe: false,
  classWhitelist: require('../data/class_whitelist.json'),
}, container));
