// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

require('../web_catalog/api_extractor.es6.js');
require('../web_apis/release.es6.js');
require('../web_apis/web_interface.es6.js');
require('../web_apis/release_interface_relationship.es6.js');
require('../web_apis/api_importer.es6.js');
require('./rate_limited_DAO.es6.js');
const objectGraph = require('object-graph-js').ObjectGraph;
const fs = require('fs');
const path = require('path');

foam.CLASS({
  name: 'DatastoreImporter',
  package: 'org.chromium.apis.web',
  documentation: `Google Cloud Datastore importer handles importing
      releaseAPIs from object graph files to a Google Cloud Datastore.`,
  imports: ['info'],
  requires: [
    'org.chromium.apis.web.ApiExtractor',
    'org.chromium.apis.web.ApiImporter',
    'com.google.cloud.datastore.DatastoreDAO',
    'org.chromium.apis.web.RateLimitedDAO',
    'org.chromium.apis.web.ReleaseWebInterfaceJunction',
    'org.chromium.apis.web.Release',
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
      documentation: 'Imports interface catalog to DAO with rateLimiter.',
      factory: function() {
        let apiImporter = this.ApiImporter.create();
        let config = {
          projectId: this.projectId,
          protocol: this.protocol,
          host: this.host,
          port: this.port,
        };
        // TODO: If all these components share several configuration values,
        // should they perhaps be configured via the context?
        apiImporter.releaseWebInterfaceJunctionDAO = this.RateLimitedDAO.create({
          delegate: this.DatastoreDAO.create(
            Object.assign({
              of: this.ReleaseWebInterfaceJunction,
            }, config)
          ),
        });
        apiImporter.releaseDAO = this.RateLimitedDAO.create({
          delegate: this.DatastoreDAO.create(
            Object.assign({
              of: this.Release,
            }, config)
          ),
        });
        apiImporter.webInterfaceDAO = this.RateLimitedDAO.create({
          delegate: this.DatastoreDAO.create(
            Object.assign({
              of: this.WebInterface,
            }, config)
          ),
        });
        return apiImporter;
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
                this.apiExtractor.extractWebCatalog(
                    objectGraph.fromJSON(JSON.parse(fs.readFileSync(
                        filePath))))));
          }
        }
        return Promise.all(promises);
      },
    },
  ],
});
