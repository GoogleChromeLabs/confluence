// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

foam.CLASS({
  package: 'org.chromium.apis.web',
  name: 'ConfluenceSyncDAO',
  extends: 'foam.dao.ProxyDAO',

  requires: [
    'foam.dao.JDAO',
    'foam.dao.MDAO',
    'foam.dao.NodeFileJournal',
    'foam.dao.SyncDAO',
    'foam.dao.sync.VersionedSyncRecord',
  ],

  properties: [
    {
      name: 'delegate',
      transient: true,
      factory: function() {
        this.validate();

        // Immediate delegate is foam.dao.SyncDAO; exposes "synced" Promise.
        return this.SyncDAO.create({
          of: this.of,
          delegate: this.JDAO.create({
            of: this.of,
            delegate: this.MDAO.create({of: this.of}),
            journal: this.NodeFileJournal.create({
              of: this.of,
              fd: this.getDataJournalFD_(),
            }),
          }),
          syncRecordDAO: this.MDAO.create({of: this.of}),
          remoteDAO: this.remoteDAO,
          polling: true,
          pollingFrequency: 1000 * 60 * 60,
        });
      },
    },
    {
      class: 'foam.dao.DAOProperty',
      name: 'remoteDAO',
      required: true,
    }
  ],

  methods: [
    {
      name: 'hasSynced',
      documentation: 'Provides API to delegate "synced" Promise.',
      returns: 'Promise',
      code: function() { return this.delegate.synced; },
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
    {
      name: 'select_',
      returns: 'Promise',
      args: [ 'x', 'sink', 'skip', 'limit', 'order', 'predicate' ],
      code: function select_() {
        console.log('SELECT', require('process').pid);
        return this.SUPER.apply(this, arguments);
      },
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
