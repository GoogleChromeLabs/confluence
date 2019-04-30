// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

require('../data_source.es6.js');
require('../fork.es6.js');
require('../web_apis/api_importer.es6.js');
require('../web_apis/release.es6.js');
require('../web_apis/release_interface_relationship.es6.js');
require('../web_apis/web_interface.es6.js');
require('./api_extractor.es6.js');
require('./api_extractor_service.es6.js');

const objectGraph = require('object-graph-js').BrowserObjectGraph;
const fs = require('fs');
const path = require('path');

foam.CLASS({
  name: 'ObjectGraphImporter',
  package: 'org.chromium.apis.web',
  implements: ['org.chromium.apis.web.ForkBoxFactory'],

  documentation: `Object graph importer handles importing releases' APIs from
      object graph files.`,

  requires: [
    'foam.box.BoxRegistry',
    'foam.box.BroadcastRegistry',
    'foam.box.Context',
    'foam.box.RegisterSelfMessage',
    'foam.box.SkeletonBox',
    'foam.core.StubFactorySingleton',
    'org.chromium.apis.web.ApiExtractor',
    'org.chromium.apis.web.ApiExtractorService',
    'org.chromium.apis.web.ApiImporter',
    'org.chromium.apis.web.Release',
    'org.chromium.apis.web.ReleaseWebInterfaceJunction',
    'org.chromium.apis.web.WebInterface',
  ],
  imports: ['info'],

  properties: [
    {
      class: 'Enum',
      of: 'org.chromium.apis.web.DataSource',
      name: 'mode',
      required: true,
    },
    {
      name: 'apiImporter',
      documentation: 'Imports interface catalog to DAO.',
      factory: function() {
        return this.ApiImporter.create();
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
      name: 'numWorkers',
      value: 16,
    },
    {
      name: 'stubFactory',
      factory: function() {
        return this.StubFactorySingleton.create();
      },
    },
    {
      class: 'FObjectProperty',
      of: 'org.chromium.apis.web.ApiExtractorService',
      name: 'apiExtractorService',
      factory: function() {
        return this.stubFactory.get(this.ApiExtractorService).create({
          delegate: this.apiContext_.registry.register(
              null, null, this.SkeletonBox.create({
                data: this.ApiExtractorService.create(),
              })),
        }, this.containerContext_);
      },
    },
    {
      class: 'FObjectProperty',
      of: 'foam.box.Context',
      name: 'containerContext_',
      factory: function() {
        return this.Context.create({
          unsafe: false,
          classWhitelist: require('../../data/class_whitelist.json'),
        });
      },
    },
    {
      class: 'FObjectProperty',
      of: 'foam.box.Context',
      name: 'apiContext_',
      factory: function() {
        this.validate();

        const ctx = this.Context.create({
          unsafe: false,
          classWhitelist: require('../../data/class_whitelist.json'),
        });
        ctx.registry = this.BroadcastRegistry.create(null, ctx);
        const numWorkers = this.numWorkers;
        const workers = new Array(numWorkers);

        for (let i = 0; i < numWorkers; i++) {
          // Stub BoxRegistry interface over box addressed to new fork.
          workers[i] = this.stubFactory.get(this.BoxRegistry).create({
            delegate: this.getForkBox(this.mode, this.containerContext_),
          }, this.containerContext_);
        }
        ctx.registry.delegates = workers;

        return ctx;
      },
    },
  ],

  methods: [
    {
      name: 'import',
      documentation: `Reads object graph files from objectGraphPath
          and extract web interfaces and import it to cloudstoreDAO
          using apiImporter.`,
      code: function() {
        const ogFileRegExp = /window_.*[.]json$/;
        const promises = [];
        const objectGraphFiles = fs.readdirSync(this.objectGraphPath);
        for (let i = 0; i < objectGraphFiles.length; i++) {
          const filePath = path.join(this.objectGraphPath, objectGraphFiles[i]);
          const stat = fs.statSync(filePath);
          if (stat.isFile()) {
            if (!ogFileRegExp.test(objectGraphFiles[i])) continue;

            let releaseInfo; // Span across f, g in .then(f).then(g).
            this.info(`Extracting APIs from ${filePath}`);
            promises.push(
                this.apiExtractorService.extractWebCatalog(filePath)
                    .then((data) => {
                      releaseInfo = data.releaseInfo;
                      this.info(`Importing APIs
                      ${releaseInfo.browser.name} ${releaseInfo.browser.version}
                      ${releaseInfo.platform.name}
                      ${releaseInfo.platform.version}`);
                      return this.apiImporter.import(
                          releaseInfo.browser.name, releaseInfo.browser.version,
                          releaseInfo.platform.name, releaseInfo.platform.version,
                          data.catalog);
                    }).then(() => {
                      this.info(`APIs imported for
                      ${releaseInfo.browser.name} ${releaseInfo.browser.version}
                      ${releaseInfo.platform.name}
                      ${releaseInfo.platform.version}`);
                    }));
          }
        }
        return Promise.all(promises);
      },
    },
    function getForkNodeParams_() {
      return [];
    },
    function getForkScriptPath_() {
      return path.resolve(`${__dirname}/../../main/forkScript.js`);
    },
  ],
});
