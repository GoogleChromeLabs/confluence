// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

global.FOAM_FLAGS = {gcloud: true};
require('foam2');
require('../lib/web_apis/api_importer.es6');
require('../lib/web_catalog/api_extractor.es6');


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

server.exportFile('/', `${__dirname}/../static/index.html`);

server.exportDAO(apiImporter.browserDAO, '/browsers');
server.exportDAO(apiImporter.interfaceDAO, '/web-interfaces');
server.exportDAO(apiImporter.browserApiDAO, '/browser-apis');

server.exportDirectory('/images', `${__dirname}/../static/images`);

server.exportDirectory('/', `${__dirname}/../static/bundle`);

server.start();
