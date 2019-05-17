// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

require('../confluence/api_count_data.es6.js');
require('../confluence/browser_metric_data.es6.js');
require('../data_source.es6.js');
require('../fork.es6.js');
require('../web_apis/release.es6.js');
require('../web_apis/release_interface_relationship.es6.js');
require('../web_apis/web_interface.es6.js');
require('./dao_container.es6.js');
require('./http_json_dao.es6.js');
require('./local_json_dao.es6.js');

foam.CLASS({
  name: 'JsonDAOContainer',
  package: 'org.chromium.apis.web',
  extends: 'org.chromium.apis.web.DAOContainer',
  implements: ['org.chromium.apis.web.ForkBoxFactory'],

  requires: [
    'foam.box.BoxRegistry',
    'foam.box.Context',
    'foam.box.SkeletonBox',
    'foam.core.StubFactorySingleton',
    'foam.dao.ClientDAO',
    'foam.dao.ReadOnlyDAO',
    'org.chromium.apis.web.ApiCountData',
    'org.chromium.apis.web.BrowserMetricData',
    'org.chromium.apis.web.DataSource',
    'org.chromium.apis.web.Release',
    'org.chromium.apis.web.ReleaseWebInterfaceJunction',
    'org.chromium.apis.web.SerializableHttpJsonDAO',
    'org.chromium.apis.web.SerializableLocalJsonDAO',
    'org.chromium.apis.web.WebInterface',
    'org.chromium.apis.web.generated.CompatData',
  ],
  imports: [
    'error',
    'info',
  ],
  exports: ['as container'],

  properties: [
    {
      class: 'Enum',
      of: 'org.chromium.apis.web.DataSource',
      name: 'mode',
      required: true,
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
      name: 'compatDAO',
      factory: function() {
        return this.getDAO(this.CompatData);
      },
    },
    {
      name: 'browserMetricsDAO',
      factory: function() {
        return this.getDAO(this.BrowserMetricData);
      },
    },
    {
      name: 'apiCountDAO',
      factory: function() {
        return this.getDAO(this.ApiCountData);
      },
    },
    {
      name: 'ctx',
      factory: function() {
        const baseCtx = this.__subContext__;
        return baseCtx.createSubContext(this.Context.create({
          unsafe: false,
          classWhitelist: require('../../data/class_whitelist.json'),
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
      this.info(`JsonDaoContainer: Creating DAO for ${cls.id}`);
      this.validate();

      const ctx = this.ctx;
      let serializableDAO;
      if (this.mode === this.DataSource.HTTP) {
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
      // Set propertyIndexGroups after other properties; its validation
      // procedure depends on other properties.
      serializableDAO.propertyIndexGroups = this.getPropertyIndexGroups(cls);

      const dao = this.ReadOnlyDAO.create({
        of: cls,
        delegate: this.ClientDAO.create({
          of: cls,
          // Create fork, and stub its BoxRegistry.
          delegate: this.stubFactory.get(this.BoxRegistry).create({
            delegate: this.getForkBox(this.mode, ctx),

          // Register skeleton over serializable DAO.
          // ClientDAO's delegate is box returned from register() call.
          }, ctx).register(null, null, this.SkeletonBox.create({
            data: serializableDAO,
          })),
        }, ctx),
      }, ctx);

      // Perform simple query to ensure DAO is eagerly initialized.
      dao.limit(1).select().then(() => {
        this.info(`JsonDaoContainer: DAO ready (${cls.id})`);
      });

      return dao;
    },
    function getPropertyIndexGroups(cls) {
      // Non-primary-key indices are:
      // ReleaseWebInterfaceJunction.sourceId: IN(SOURCE_ID, [releases]) queries
      if (cls !== this.ReleaseWebInterfaceJunction) return [];
      return ['sourceId'];
    },
    function getForkNodeParams_() {
      return ['--max_old_space_size=4096'];
    },
    function getForkScriptPath_() {
      return require('path').resolve(`${__dirname}/../../main/forkScript.js`);
    },
  ],
});
