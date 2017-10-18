// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

require('../web_apis/api_importer.es6.js');
require('../web_apis/release.es6.js');
require('../web_apis/release_interface_relationship.es6.js');
require('../web_apis/web_interface.es6.js');
require('./api_extractor.es6.js');

const objectGraph = require('object-graph-js').ObjectGraph;
const fs = require('fs');
const path = require('path');

foam.CLASS({
  name: 'ObjectGraphImporter',
  package: 'org.chromium.apis.web',

  documentation: `Object graph importer handles importing releases' APIs from
      object graph files.`,

  requires: [
    'org.chromium.apis.web.ApiExtractor',
    'org.chromium.apis.web.ApiImporter',
    'org.chromium.apis.web.ReleaseWebInterfaceJunction',
    'org.chromium.apis.web.Release',
    'org.chromium.apis.web.WebInterface',
  ],
  imports: ['info'],

  properties: [
    {
      name: 'apiImporter',
      documentation: 'Imports interface catalog to DAO.',
      factory: function() { return this.ApiImporter.create(); },
    },
    {
      class: 'String',
      name: 'objectGraphPath',
      documentation: `The path to the directory containing object
          graph files. All files with name starting with "window_"
          are loaded as object graph files in this directory. Directories
          or files not starting with "window_" are ignored.`,
      required: true,
      final: true,
    },
  ],
  methods: [
    {
      name: 'import',
      documentation: `Reads object graph files from objectGraphPath
          and extract web interfaces and import it to cloudstoreDAO
          using apiImporter.`,
      code: function() {
        let promises = [];
        let objectGraphFiles = fs.readdirSync(this.objectGraphPath);
        for (let i = 0; i < objectGraphFiles.length; i++) {
          let filePath = path.join(this.objectGraphPath, objectGraphFiles[i]);
          let stat = fs.statSync(filePath);
          if (stat.isFile()) {
            let releaseInfo = objectGraphFiles[i].slice(0, -5).split('_');
            // All Object Graph files' filename starts with "window_".
            if (releaseInfo[0] !== 'window') continue;
            this.info(`read og file: ${objectGraphFiles[i]}`);
            let bName = releaseInfo[1];
            let bVersion = releaseInfo[2];
            let osName = releaseInfo[3];
            let osVersion = releaseInfo[4];
            promises.push(this.apiImporter.import(
                bName, bVersion, osName, osVersion,
                this.ApiExtractor.create({
                  objectGraph: objectGraph.fromJSON(JSON.parse(fs.readFileSync(
                      filePath)))
                }).extractWebCatalog()));
          }
        }
        return Promise.all(promises);
      },
    },
  ],
});
