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
require('../lib/json_dao_container.es6.js');
require('../lib/web_apis/release.es6.js');
require('../lib/web_apis/release_interface_relationship.es6.js');
require('../lib/web_apis/web_interface.es6.js');

const logger = foam.log.ConsoleLogger.create();
foam.box.node.ForkBox.CONNECT_TO_PARENT(logger);
