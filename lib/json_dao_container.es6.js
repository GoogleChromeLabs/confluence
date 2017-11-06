// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

require('./confluence/api_velocity_data.es6.js');
require('./confluence/browser_metric_data.es6.js');
require('./dao_container.es6.js');
require('./http_json_dao.es6.js');
require('./local_json_dao.es6.js');
require('./web_apis/release.es6.js');
require('./web_apis/release_interface_relationship.es6.js');
require('./web_apis/web_interface.es6.js');

foam.ENUM({
  name: 'JsonDAOContainerMode',
  package: 'org.chromium.apis.web',

  documentation: `Indicator for what "mode" a JsonDAOContainer should run in.`,

  values: [
    {
      name: 'LOCAL',
      documentation: 'JSON to be loaded from the local filesystem.',
    },
    {
      name: 'HTTP',
      documentation: 'JSON to be loaded via HTTP GET.'
    },
  ],
});

foam.CLASS({
  name: 'JsonDAOContainer',
  package: 'org.chromium.apis.web',
  extends: 'org.chromium.apis.web.DAOContainer',

  requires: [
    'foam.box.BoxRegistry',
    'foam.box.Context',
    'foam.box.SkeletonBox',
    'foam.box.node.ForkBox',
    'foam.core.StubFactorySingleton',
    'foam.dao.ClientDAO',
    'foam.dao.ReadOnlyDAO',
    'org.chromium.apis.web.ApiVelocityData',
    'org.chromium.apis.web.BrowserMetricData',
    'org.chromium.apis.web.JsonDAOContainerMode',
    'org.chromium.apis.web.Release',
    'org.chromium.apis.web.ReleaseWebInterfaceJunction',
    'org.chromium.apis.web.SerializableHttpJsonDAO',
    'org.chromium.apis.web.SerializableLocalJsonDAO',
    'org.chromium.apis.web.WebInterface',
  ],
  imports: ['info'],
  exports: ['as container'],

  properties: [
    {
      class: 'Enum',
      of: 'org.chromium.apis.web.JsonDAOContainerMode',
      name: 'mode',
    },
    {
      class: 'String',
      documentation: `A prefix for either local file paths or URLs.
          Interpretation depends on "mode" value.`,
      name: 'basename',
    },
    {
      name: 'releaseDAO',
      factory: function() {
        return this.getDAO(this.Release);
      },
    },
    {
      name: 'webInterfaceDAO',
      factory: function() {
        return this.getDAO(this.WebInterface);
      },
    },
    {
      name: 'releaseWebInterfaceJunctionDAO',
      factory: function() {
        return this.getDAO(this.ReleaseWebInterfaceJunction);
      },
    },
    {
      name: 'browserMetricsDAO',
      factory: function() {
        return this.getDAO(this.BrowserMetricData);
      },
    },
    {
      name: 'apiVelocityDAO',
      factory: function() {
        return this.getDAO(this.ApiVelocityData);
      },
    },
    {
      name: 'ctx',
      factory: function() {
        const baseCtx = this.__subContext__;
        return baseCtx.createSubContext(this.Context.create({
          unsafe: false,
          classWhitelist: require('../data/class_whitelist.json'),
        }, baseCtx));
      },
    },
    {
      name: 'stubFactory',
      factory: function() {
        return this.StubFactorySingleton.create();
      },
    },
  ],

  methods: [
    function getDAO(cls) {
      const ctx = this.ctx;
      let serializableDAO;
      if (this.mode === this.JsonDAOContainerMode.HTTP) {
        serializableDAO = this.SerializableHttpJsonDAO.create({
          of: cls,
          url: `${this.basename}/${cls.id}.json`,
        }, ctx);
      } else {
        serializableDAO = this.SerializableLocalJsonDAO.create({
          of: cls,
          path: `${this.basename}/${cls.id}.json`,
        }, ctx);
      }

      return this.ReadOnlyDAO.create({
        of: cls,
        delegate: this.ClientDAO.create({
          of: cls,
          // Create fork, and stub its BoxRegistry.
          delegate: this.stubFactory.get(this.BoxRegistry).create({
            delegate: this.ForkBox.create({
              critical: true,
              nodeParams: this.getForkNodeParams_(),
              childScriptPath: this.getForkScriptPath_(),
            }, ctx),

          // Register skeleton over serializable DAO.
          // ClientDAO's delegate is box returned from register() call.
          }, ctx).register(null, null, this.SkeletonBox.create({
            data: serializableDAO,
          })),
        }, ctx),
      });
    },
    function getForkNodeParams_() { return ['--max_old_space_size=4096']; },
    function getForkScriptPath_() {
      return require('path').resolve(`${__dirname}/../main/forkScript.js`);
    },
  ],
});
