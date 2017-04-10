// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

global.FOAM_FLAGS = {gcloud: true};
require('foam2');
require('../lib/web_apis/api_importer.es6');
require('../lib/web_catalog/api_extractor.es6');
require('../lib/confluence/api_velocity.es6.js');
require('../lib/confluence/failure_to_ship.es6.js');
require('../lib/confluence/vendor_specific.es6.js');
require('../lib/confluence/aggressive_removal.es6.js');


let server = foam.net.node.Server.create({
  port: 9000,
});

// Use object graph data from ../data/og directory for test
// before cloudstore DAO is done.
const OG_DATA_PATH = `${__dirname}/../data/og`;
let extractor = org.chromium.apis.web.apiExtractor.create({});
let apiImporter = org.chromium.apis.web.ApiImporter.create({});
const objectGraph = require('object-graph-js').ObjectGraph;
const fs = require('fs');
let ogFiles = fs.readdirSync(OG_DATA_PATH);

for (let i = 0; i < ogFiles.length; i += 1) {
  console.log(`read object graph file ${ogFiles[i]}`);
  let filePath = `${OG_DATA_PATH}/${ogFiles[i]}`;
  let stat = fs.statSync(filePath);
  if (stat.isFile()) {
    let browserInfo = ogFiles[i].slice(0, -5).split('_');
    // Object graph files starts with "window_".
    if (browserInfo[0] !== 'window') continue;
    apiImporter.import(browserInfo[1], browserInfo[2], browserInfo[3],
      browserInfo[4], extractor.extractWebCatalog(objectGraph
      .fromJSON(JSON.parse(fs.readFileSync(filePath)))));
  }
}

let apiVelocity = org.chromium.apis.web.ApiVelocity.create({
  browserDAO: apiImporter.browserDAO,
  interfaceDAO: apiImporter.interfaceDAO,
  browserApiDAO: apiImporter.browserApiDAO,
});

let failureToShip = org.chromium.apis.web.FailureToShip.create({
  browserDAO: apiImporter.browserDAO,
  interfaceDAO: apiImporter.interfaceDAO,
  browserApiDAO: apiImporter.browserApiDAO,
});

let vendorSpecific = org.chromium.apis.web.VendorSpecific.create({
  browserDAO: apiImporter.browserDAO,
  interfaceDAO: apiImporter.interfaceDAO,
  browserApiDAO: apiImporter.browserApiDAO,
});

let aggressiveRemoval = org.chromium.apis.web.AggressiveRemoval.create({
  browserDAO: apiImporter.browserDAO,
  interfaceDAO: apiImporter.interfaceDAO,
  browserApiDAO: apiImporter.browserApiDAO,
});

server.exportFile('/', `${__dirname}/../static/index.html`);

server.exportDAO(apiImporter.browserDAO, '/browsers');
server.exportDAO(apiImporter.interfaceDAO, '/web-interfaces');
server.exportDAO(apiImporter.browserApiDAO, '/browser-apis');
server.exportDAO(apiVelocity.apiVelocityDAO, '/api-velocity');
server.exportDAO(failureToShip.failureToShipDAO, '/failure-to-ship');
server.exportDAO(vendorSpecific.vendorSpecificDAO, '/vendor-specific');
server.exportDAO(aggressiveRemoval.aggressiveRemovalDAO, '/aggressive-removal');

server.exportDirectory('/images', `${__dirname}/../static/images`);

server.exportDirectory('/', `${__dirname}/../static/bundle`);

server.start();
