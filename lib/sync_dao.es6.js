// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

require('./web_apis/release_interface_relationship.es6.js');
const pkg = org.chromium.apis.web;

foam.CLASS({
  package: 'org.chromium.apis.web',
  name: 'SyncDAOMonitor',
  implements: ['foam.mlang.Expressions'],

  imports: ['info'],

  properties: [
    {
      class: 'foam.dao.DAOProperty',
      name: 'dao',
      required: true,
      postSet: function(old, nu) {
        if (!nu) return;
        foam.assert(nu.synced && nu.synced.then,
                    'SyncDAOMonitor expects "dao.synced" to be thenable');
        foam.assert(nu.synced$ && nu.synced$.sub,
                    'SyncDAOMonitor expects "dao.synced$" to be subscribable');
        if (this.syncedSub_) this.syncedSub_.detach();
        this.syncedSub_ = nu.synced$.sub(this.onSyncedChanged);
        // Synthetic "synced" property propertyChange-event.
        this.onSyncedChanged(
            this.syncedSub_, 'propertyChange', 'synced', nu.synced$);
      },
    },
    {
      class: 'String',
      name: 'name',
      expression: function(dao) {
        return dao && dao.of && dao.of.id ? dao.of.id :
            'synced data';
      },
    },
    {
      class: 'Int',
      name: 'latestVersion_',
    },
  ],

  listeners: [
    function onSyncedChanged(_, __, ___, synced$) {
      synced$.get().then(this.onSynced);
    },
    function onSynced() {
      this.dao
          // Like MAX(), but faster on DAOs that can optimize order+limit.
          .orderBy(this.DESC(this.dao.of.VERSION_)).limit(1).select()
          .then(this.onSyncedSelect);
    },
    function onSyncedSelect(arraySink) {
      const latestRecord = arraySink.array[0];
      if (!latestRecord) return;
      const latestVersion = latestRecord.version_;
      foam.assert(latestVersion >= this.latestVersion_,
                  'SyncDAOMonitor expects version to increase monotonically');

      if (this.dao.polling) {
        this.info(`DAO, ${this.name}, synced
                       (polling every ${this.dao.pollingFrequency}ms)`);
      }

      if (latestVersion > this.latestVersion_) {
        this.info(`DAO, ${this.name}, synced from version ${this.latestVersion_}
                       to ${latestVersion}`);
        this.latestVersion_ = latestVersion;
      }
    },
  ],
});

foam.CLASS({
  package: 'org.chromium.apis.web',
  name: 'ConfluenceSyncDAO',
  extends: 'foam.dao.ProxyDAO',

  requires: [
    'com.google.cloud.datastore.BatchMutationDatastoreDAO',
    'foam.dao.JDAO',
    'foam.dao.MDAO',
    'foam.dao.NodeFileJournal',
    'foam.dao.SyncDAO',
    'foam.dao.sync.VersionedSyncRecord',
    'org.chromium.apis.web.SyncDAOMonitor',
    'org.chromium.apis.web.VersionedReleaseWebInterfaceJunction',
  ],
  imports: ['container'],

  properties: [
    {
      name: 'delegate',
      transient: true,
      factory: function() {
        this.validate();

        // TODO(markdittmer): Clean this up. Need a way to override lookup and
        // requiresCreate.
        const lookup = this.lookup.bind(this);

        const indexedMDAO = lookup('foam.dao.MDAO').create({
          of: this.of,
        }, this.container.ctx);
        // Index releaes ID on release/interface junction DAO.
        if (this.of === pkg.VersionedReleaseWebInterfaceJunction) {
          console.log('ADDING SOURCE_ID INDEX');
          indexedMDAO.addPropertyIndex(
              pkg.ReleaseWebInterfaceJunction.SOURCE_ID);
        }

        // Immediate delegate is foam.dao.SyncDAO; exposes "synced" Promise.
        const delegate = lookup('foam.dao.SyncDAO').create({
          of: this.of,
          delegate: lookup('foam.dao.JDAO').create({
            of: this.of,
            delegate: indexedMDAO,
            journal: lookup('foam.dao.NodeFileJournal').create({
              of: this.of,
              fd: this.getDataJournalFD_(),
            }, this.container.ctx),
          }, this.container.ctx),
          syncRecordDAO: lookup('foam.dao.MDAO').create({
            of: this.of,
          }, this.container.ctx),
          remoteDAO: this.remoteDAO,
          polling: true,
          pollingFrequency: 1000 * 60 * 60,
        }, this.container.ctx);

        // Point monitor at delegate.
        this.monitor_.dao = delegate;

        return delegate;
      },
    },
    {
      class: 'foam.dao.DAOProperty',
      name: 'remoteDAO',
      transient: true,
      factory: function() {
        // TODO(markdittmer): Clean this up. Need a way to override lookup and
        // requiresCreate.
        const lookup = this.lookup.bind(this);
        return lookup('foam.dao.NoDisjunctionDAO').create({
          of: this.of,
          delegate: lookup(
            'com.google.cloud.datastore.BatchMutationDatastoreDAO').create({
              of: this.of,
              numBatches: 25,
            }, this.container.ctx),
        }, this.container.ctx);
      },
    },
    {
      class: 'FObjectProperty',
      of: 'org.chromium.apis.web.SyncDAOMonitor',
      name: 'monitor_',
      transient: true,
      factory: function() {
        return foam.lookup('org.chromium.apis.web.SyncDAOMonitor')
            .create(null, this.container.ctx);
      },
    },
  ],

  methods: [
    {
      name: 'hasSynced',
      documentation: 'Provides API to delegate "synced" Promise.',
      returns: 'Promise',
      code: function() { return this.delegate.synced; },
    },
    {
      name: 'lookup',
      documentation: 'Perform lookup on container context.',
      code: function(id) {
        return this.container.ctx.lookup(id);
      }
    },
    function getDataJournalFD_() {
      return require('fs').openSync(
          require('path').resolve(
              __dirname,
              `../data/${this.of.id}-journal.js`),
          'r');
    },
    function getSyncRecordFD_() {
      return require('fs').openSync(
          require('path').resolve(
              __dirname,
              `../data/foam.dao.sync.VersionedSyncRecord-${this.of.id}-journal.js`),
          'r');
    },
  ],
});

foam.CLASS({
  package: 'org.chromium.apis.web',
  name: 'ConfluenceBaseClientDAO',
  extends: 'foam.dao.AbstractDAO',

  documentation: 'Like foam.dao.BaseClientDAO, but stubs "hasSynced".',

  properties: [
    {
      class: 'Stub',
      of: 'org.chromium.apis.web.ConfluenceSyncDAO',
      name: 'delegate',
      methods: [
        'hasSynced',
        'put_',
        'remove_',
        'removeAll_',
        'select_',
        'listen_',
        'find_',
      ],
    },
  ],
});

foam.CLASS({
  package: 'org.chromium.apis.web',
  name: 'ConfluenceClientDAO',
  extends: 'org.chromium.apis.web.ConfluenceBaseClientDAO',
  implements: ['foam.dao.ClientDAO'],

  documentation: 'Like foam.dao.ClientDAO, but stubs "hasSynced".',
});
