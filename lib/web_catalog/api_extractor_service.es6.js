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
          const apiExtractor = ApiExtractor.create({
            objectGraph: ObjectGraph.fromJSON(require(objectGraphFilePath)),
          });
          this.info(`Extracting web catalog for ${objectGraphFilePath}`);
          const catalog = apiExtractor.extractWebCatalog();
          this.info(`Web catalog extracted for ${objectGraphFilePath}`);
          return Promise.resolve(catalog);
        } catch (error) {
          return Promise.reject(error);
        }
      },
    },
  ],
});
