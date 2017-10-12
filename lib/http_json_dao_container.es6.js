// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

require('./confluence/api_velocity_data.es6.js');
require('./confluence/browser_metric_data.es6.js');
require('./dao_container.es6.js');
require('./sync_dao.es6.js');
require('./web_apis/release.es6.js');
require('./web_apis/release_interface_relationship.es6.js');
require('./web_apis/web_interface.es6.js');

foam.CLASS({
  name: 'HttpJsonDAOContainer',
  package: 'org.chromium.apis.web',
  extends: 'org.chromium.apis.web.DAOContainer',

  requires: [
    'foam.box.BoxRegistry',
    'foam.box.Context',
    'foam.box.SkeletonBox',
    'foam.box.node.ForkBox',
    'foam.core.StubFactorySingleton',
    'foam.dao.ReadOnlyDAO',
    'org.chromium.apis.web.ConfluenceClientDAO',
    'org.chromium.apis.web.ConfluencePollingDAO',
  ],
  imports: ['info'],
  exports: ['as container'],

  properties: [
    {
      name: 'stubFactory',
      factory: function() {
        return this.StubFactorySingleton.create();
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
  ],

  methods: [
    function getDAO(cls) {
      const ctx = this.ctx;
        let dao = this.ReadOnlyDAO.create({of: cls});
        let clientDAO = this.ConfluenceClientDAO.create({
          of: cls,
          // Create fork, and stub its BoxRegistry.
          delegate: this.stubFactory.get(this.BoxRegistry).create({
            delegate: this.ForkBox.create({
              nodeParams: this.getForkNodeParams_(),
              childScriptPath: this.getForkScriptPath_(),
            }, ctx),
            // Register skeleton over ConfluenceSyncDAO. ConfluenceClientDAO's
            // delegate is box returned from register() call.
          }, ctx).register(null, null, this.SkeletonBox.create({
            data: this.ConfluencePollingDAO.create({
              of: cls,
              url: `https://storage.googleapis.com/web-api-confluence-data-cache/latest/json/${cls.id}.json`,
            }, ctx),
          })),
        }, ctx);
        dao.delegate = clientDAO;
        this.info(`Loading read-only DAO of ${cls.id}`);
        clientDAO.hasSynced().then(function() {
          this.info(`Serving read-only DAO of ${cls.id} from memory`);
        }.bind(this));
        return dao;
    },
    function getForkNodeParams_() { return ['--max_old_space_size=4096']; },
    function getForkScriptPath_() {
      return require('path').resolve(`${__dirname}/../main/forkScript.js`);
    },
  ],
});
