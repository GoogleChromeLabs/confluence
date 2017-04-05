// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

global.FOAM_FLAGS = {gcloud: true};
require('foam2');
require('../web_catalog/api_extractor.es6.js');
require('../web_apis/browser.es6.js');
require('../web_apis/web_interface.es6.js');
require('../web_apis/browser_interface_relationship.es6.js');
require('../web_apis/api_importer.es6.js');
const objectGraph = require('object-graph-js').ObjectGraph;
const fs = require('fs');

foam.CLASS({
  name: 'cloudstoreImporter',
  package: 'org.chromium.apis.web',
  documentation: `Google Cloud Datastore importer handles importing
      browserAPIs from object graph files to a Google Cloud Datastore.`,
  requires: [
    'org.chromium.apis.web.apiExtractor',
    'org.chromium.apis.web.ApiImporter',
    'com.google.cloud.datastore.DatastoreDAO',
    'org.chromium.apis.web.BrowserWebInterfaceJunction',
    'org.chromium.apis.web.Browser',
    'org.chromium.apis.web.WebInterface',
  ],
  properties: [
    {
      name: 'extractor',
      documentation: `Extracts interface catalog from object
          graph object.`,
      factory: function() {
        return this.apiExtractor.create();
      },
    },
    {
      name: 'apiImporter',
      documentation: `Imports interface catalog to cloudStore DAO.`,
      factory: function() {
        return this.ApiImporter.create({
          toCloudDatastore: true,
        });
      },
    },
    {
      name: 'objectGraphPath',
      documentation: `The path to the directory containing object
          graph files.`,
      required: true,
      final: true,
    },
    {
      name: 'projectId',
      documentation: `This project's id in Google Cloud Platform.`,
      required: true,
      final: true,
    },
    {
      name: 'protocol',
      documentation: `Protocol of connecting to the datastore.
          Use default protocol in DatastoreDAO if not specified.`,
      value: null,
    },
    {
      name: 'host',
      documentation: `host of datastore. Use default protocol in
          DatastoreDAO if not specified.`,
      value: null,
    },
    {
      name: 'port',
      documentation: `port of connecting to the datastore. Use
          default port in DatastoreDAO if not specified.`,
      value: null,
    },
  ],
  methods: [
    {
      name: 'init',
      documentation: `Reads object graph files from objectGraphPath
          and extract web interfaces and import it to cloudstoreDAO
          using apiImporter.`,
      code: function() {
        let ogFiles = fs.readdirSync(this.objectGraphPath);
        let config = {
          projectId: this.projectId,
        };
        if (this.protocol !== null) config.protocol = this.protocol;
        if (this.host !== null) config.host = this.host;
        if (this.port !== null) config.port = this.port;
        this.apiImporter.browserApiDAO = this.DatastoreDAO.create(
          Object.assign({
            of: this.BrowserWebInterfaceJunction,
          }, config)
        );
        this.apiImporter.browserDAO = this.DatastoreDAO.create(
          Object.assign({
            of: this.Browser,
          }, config)
        );
        this.apiImporter.interfaceDAO = this.DatastoreDAO.create(
          Object.assign({
            of: this.WebInterface,
          }, config)
        );
        for (let i = 0; i < ogFiles.length; i++) {
          let filePath = `${this.objectGraphPath}/${ogFiles[i]}`;
          let stat = fs.statSync(filePath);
          if (stat.isFile()) {
            let browserInfo = ogFiles[i].slice(0, -5).split('_');
            if (browserInfo[0] !== 'window') return;
            console.info(`read og file: ${ogFiles[i]}`);
            let bName = browserInfo[1];
            let bVersion = browserInfo[2];
            let osName = browserInfo[3];
            let osVersion = browserInfo[4];
            this.apiImporter.import(bName, bVersion, osName, osVersion,
              this.extractor.extractWebCatalog(
                objectGraph.fromJSON(JSON.parse(fs.readFileSync(filePath)))
              ));
          }
        }
      },
    },
  ],
});
