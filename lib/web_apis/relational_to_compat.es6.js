// Copyright 2018 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

require('./api_compat_data.es6.js');

foam.CLASS({
  name: 'AbstractRelationalToCompatConverter',
  package: 'org.chromium.apis.web',

  documentation: `Component that encapsulates logic for converting relational
      Release<-->API database into a unified compat data model.`,

  axioms: [foam.pattern.Singleton.create()],

  requires: [
    'org.chromium.apis.web.CompatClassGenerator',
    'org.chromium.apis.web.CompatProperty',
  ],
  imports: ['info'],

  properties: [
    {
      class: 'FObjectProperty',
      of: 'org.chromium.apis.web.AbstractCompatClassGenerator',
      name: 'classGenerator',
      factory: function() { return this.CompatClassGenerator.create(); },
    },
  ],

  methods: [
    {
      name: 'convert',
      code: function(releases, apis, joins) {
        this.info(`Generating data model for ${releases.length} releases`);
        const generator = this.classGenerator;
        const CompatData = generator.generateClass(generator.generateSpec(
            'org.chromium.apis.web.generated', 'CompatData', releases));
        this.info('Data model generated');

        this.info('Transferring relational data to new data model');
        let compat = {};
        for (const api of apis) {
          const interfaceName = api.interfaceName;
          const apiName = api.apiName;
          const compatData = CompatData.create({interfaceName, apiName});
          compat[compatData.id] = compatData;
        }

        let compatProps = {};
        CompatData.getAxiomsByClass(this.CompatProperty).forEach(prop => {
          compatProps[prop.release.id] = prop;
        });
        for (const join of joins) {
          let data = compat[join.targetId];
          foam.assert(data, `Unable to find API for join ${join.sourceId} <--> ${join.targetId}`);
          const releaseId = join.sourceId;
          const prop = compatProps[releaseId];
          foam.assert(prop, `Unable to find compatability property for release ID: ${releaseId}`);
          prop.set(data, true);
        }

        let compatArray = [];
        for (const key of Object.keys(compat)) {
          compatArray.push(compat[key]);
        }
        this.info('Relational data transferred to new data model');

        return {cls: CompatData, data: compatArray};
      },
    },
  ],
});

foam.CLASS({
  name: 'RelationalToCompatConverter',
  package: 'org.chromium.apis.web',
  extends: 'org.chromium.apis.web.AbstractRelationalToCompatConverter',

  documentation: 'Singleton AbstractRelationalToCompatConverter',

  axioms: [foam.pattern.Singleton.create()],
});
