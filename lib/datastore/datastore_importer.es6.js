// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

require('../web_catalog/api_extractor.es6.js');
require('../web_apis/browser.es6.js');
require('../web_apis/web_interface.es6.js');
require('../web_apis/browser_interface_relationship.es6.js');
require('../web_apis/api_importer.es6.js');
require('./rate_limited_DAO.es6.js');
const objectGraph = require('object-graph-js').ObjectGraph;
const fs = require('fs');

foam.CLASS({
  name: 'DatastoreImporter',
  package: 'org.chromium.apis.web',
  documentation: `Google Cloud Datastore importer handles importing
      browserAPIs from object graph files to a Google Cloud Datastore.`,
  imports: ['info'],
  requires: [
    'org.chromium.apis.web.ApiExtractor',
    'org.chromium.apis.web.ApiImporter',
    'com.google.cloud.datastore.DatastoreDAO',
    'org.chromium.apis.web.RateLimitedDAO',
    'org.chromium.apis.web.BrowserWebInterfaceJunction',
    'org.chromium.apis.web.Browser',
    'org.chromium.apis.web.WebInterface',
  ],
  properties: [
    {
      name: 'apiExtractor',
      documentation: `Extracts interface catalog from object
          graph object.`,
      factory: function() {
        return this.ApiExtractor.create();
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
      class: 'String',
      name: 'objectGraphPath',
      documentation: `The path to the directory containing object
          graph files. All files with name starting with "window_"
          are loaded as object graph files in this directory. Directories
          or files not starting with "window_" are ignored.`,
      required: true,
      final: true,
    },
    {
      class: 'String',
      name: 'projectId',
      documentation: `This project's id in Google Cloud Platform.`,
      required: true,
      final: true,
    },
    {
      class: 'String',
      name: 'protocol',
      documentation: `Protocol for connecting to the datastore.
          Default value is "https".`,
      value: 'https',
    },
    {
      class: 'String',
      name: 'host',
      documentation: `Hostname part of Datastore REST API URL.
          Default host is "datastore.googleapis.com".`,
      value: 'datastore.googleapis.com',
    },
    {
      class: 'Int',
      name: 'port',
      documentation: `Port for connecting to Datastore.
          default port is 443.`,
      value: 443,
    },
  ],
  methods: [
    {
      name: 'init',
      documentation: `Reads object graph files from objectGraphPath
          and extract web interfaces and import it to cloudstoreDAO
          using apiImporter.`,
      code: function() {
        let objectGraphFiles = fs.readdirSync(this.objectGraphPath);
        let config = {
          projectId: this.projectId,
        };
        if (this.protocol !== null) config.protocol = this.protocol;
        if (this.host !== null) config.host = this.host;
        if (this.port !== null) config.port = this.port;
        // TODO: If all these components share several configuration values,
        // should they perhaps be configured via the context?
        this.apiImporter.browserApiDAO = this.RateLimitedDAO.create({
          delegate: this.DatastoreDAO.create(
            Object.assign({
              of: this.BrowserWebInterfaceJunction,
            }, config)
          ),
        });
        this.apiImporter.browserDAO = this.RateLimitedDAO.create({
          delegate: this.DatastoreDAO.create(
            Object.assign({
              of: this.Browser,
            }, config)
          ),
        });
        this.apiImporter.interfaceDAO = this.RateLimitedDAO.create({
          delegate: this.DatastoreDAO.create(
            Object.assign({
              of: this.WebInterface,
            }, config)
          ),
        });
        for (let i = 0; i < objectGraphFiles.length; i++) {
          let filePath = `${this.objectGraphPath}/${objectGraphFiles[i]}`;
          let stat = fs.statSync(filePath);
          if (stat.isFile()) {
            let browserInfo = objectGraphFiles[i].slice(0, -5).split('_');
            // All Object Graph files' filename starts with "window_".
            if (browserInfo[0] !== 'window') continue;
            this.info(`read og file: ${objectGraphFiles[i]}`);
            let bName = browserInfo[1];
            let bVersion = browserInfo[2];
            let osName = browserInfo[3];
            let osVersion = browserInfo[4];
            this.apiImporter.import(bName, bVersion, osName, osVersion,
              this.apiExtractor.extractWebCatalog(
                objectGraph.fromJSON(JSON.parse(fs.readFileSync(filePath)))
              ));
          }
        }
      },
    },
  ],
});
