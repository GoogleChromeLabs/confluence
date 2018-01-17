// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

require('./api_extractor.es6.js');

const ObjectGraph = require('object-graph-js').ObjectGraph;

foam.CLASS({
  package: 'org.chromium.apis.web',
  name: 'ApiExtractorService',

  imports: [
    'container',
    'info',
  ],

  properties: [
    // TODO(markdittmer): This should be unnecessary. Limitations imposed during
    // deserialization should not persist after deserialized components are
    // instantiated.
    {
      name: 'localCtx',
      documentation: `Local context that escapes any whitelisted deserialization
          context service may have been created in.`,
      transient: true,
      factory: function() { return this.container.ctx || this.container; },
    },
    {
      name: 'fs',
      factory: function() { return require('fs'); },
    },
    {
      name: 'path',
      factory: function() { return require('path'); },
    },
  ],

  methods: [
    {
      name: 'extractWebCatalog',
      returns: 'Promise',
      code: function(objectGraphFilePath) {
        try {
          const ApiExtractor =
              this.localCtx.lookup('org.chromium.apis.web.ApiExtractor');
          this.info(`Loading ${objectGraphFilePath}`);

          let graphJSON;
          try {
            graphJSON = JSON.parse(this.fs.readFileSync(
                objectGraphFilePath));
          } catch (error) {
            return Promise.reject(error);
          }

          let releaseInfo;
          try {
            releaseInfo = this.getReleaseInfo(objectGraphFilePath, graphJSON);
          } catch (error) {
            return Promise.reject(error);
          }

          const apiExtractor = ApiExtractor.create({
            objectGraph: ObjectGraph.fromJSON(graphJSON),
          });
          this.info(`Extracting web catalog for ${objectGraphFilePath}`);
          const catalog = apiExtractor.extractWebCatalog();
          this.info(`Web catalog extracted for ${objectGraphFilePath}`);
          return Promise.resolve({
            releaseInfo,
            catalog,
          });
        } catch (error) {
          return Promise.reject(error);
        }
      },
    },
    function getReleaseInfo(path, json) {
      if (json.environment && json.environment.browser &&
          json.environment.browser.name && json.environment.browser.version &&
          json.environment.platform && json.environment.platform.name &&
          json.environment.platform.version) {
        return json.environment;
      } else {
        return this.getReleaseInfoFromPath_(path);
      }
    },
    function getReleaseInfoFromPath_(path) {
      const filename = this.path.basename(path);
      let ret = {browser: {}, platform: {}};

      // Drop slice(0, -5): ".json".
      foam.assert(filename.endsWith('.json'),
                  'Object graph JSON file must end with ".json"');
      const releaseInfo = filename.slice(0, -5).split('_');
      foam.assert(releaseInfo.length === 5,
                  `Object graph JSON file name expected to be of the form:
                      window_[browser-name]_[browser-version]_[platform-name][platform-version].json`);
      ret.browser.name = releaseInfo[1];
      ret.browser.version = releaseInfo[2];
      ret.platform.name = releaseInfo[3];
      ret.platform.version = releaseInfo[4];

      return ret;
    },
  ],
});
