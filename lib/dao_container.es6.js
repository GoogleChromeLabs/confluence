// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

foam.CLASS({
  name: 'DAOContainer',
  package: 'org.chromium.apis.web',

  documentation: `Container for contextualizing related DAOs:

  Release,
  WebInterface,
  Release <--> WebInterface (ReleaseWebInterfaceJunction),
  BrowserMetricData (aggressive removal, browser-specific, etc.),
  ApiVelocityData (total APIs, new APIs, removed APIs).`,

  requires: [
    'foam.dao.AdapterDAO',
    'foam.dao.JDAO',
    'foam.dao.MDAO',
    'foam.dao.NodeFileJournal',
    'foam.dao.ReadOnlyDAO',
    'foam.version.VersionedClassFactorySingleton',
    'org.chromium.apis.web.ApiVelocityData',
    'org.chromium.apis.web.BrowserMetricData',
    'org.chromium.apis.web.Release',
    'org.chromium.apis.web.ReleaseWebInterfaceJunction',
    'org.chromium.apis.web.WebInterface',
  ],
  exports: [
    'apiVelocityDAO',
    'browserMetricsDAO',
    'releaseDAO',
    'releaseWebInterfaceJunctionDAO',
    'webInterfaceDAO',
  ],

  // Names applied to DAOs on both ends of WebSocket. These names are
  // consolidated here to ensure that they match on the web client and front end
  // server.
  constants: {
    RELEASE_NAME: 'releaseDAO',
    WEB_INTERFACE_NAME: 'webInterfaceDAO',
    RELEASE_WEB_INTERFACE_JUNCTION_NAME: 'releaseWebInterfaceJunctionDAO',
    API_VELOCITY_NAME: 'apiVelocityDAO',
    FAILURE_TO_SHIP_NAME: 'failureToShipDAO',
    BROWSER_SPECIFIC_NAME: 'browserSpecificDAO',
    AGGRESSIVE_REMOVAL_NAME: 'aggressiveRemovalDAO',
  },

  properties: [
    {
      class: 'foam.dao.DAOProperty',
      documentation: 'DAO providing access to all known browser releases.',
      name: 'releaseDAO',
      factory: function() { return this.getDAO(this.Release); },
    },
    {
      class: 'foam.dao.DAOProperty',
      documentation: 'DAO providing access to all known web APIs.',
      name: 'webInterfaceDAO',
      factory: function() { return this.getDAO(this.WebInterface); },
    },
    {
      class: 'foam.dao.DAOProperty',
      documentation: `DAO providing access to all browser release <--> API
          relations.`,
      name: 'releaseWebInterfaceJunctionDAO',
      factory: function() {
        return this.getDAO(this.ReleaseWebInterfaceJunction);
      },
    },
    {
      class: 'foam.dao.DAOProperty',
      documentation: `DAO providing access to historical browser metrics data.`,
      name: 'browserMetricsDAO',
      factory: function() { return this.getDAO(this.BrowserMetricData); },
    },
    {
      class: 'foam.dao.DAOProperty',
      documentation: `DAO providing access to API Velocity metric data.`,
      name: 'apiVelocityDAO',
      factory: function() { return this.getDAO(this.ApiVelocityData); },
    },
    {
      name: 'versionedClassFactory_',
      factory: function() {
        return this.VersionedClassFactorySingleton.create();
      }
    },
  ],

  methods: [
    function getDAO(cls) {
      // Get a versioned variant of the requested class.
      const versionedCls = this.versionedClassFactory_.get(cls);

      // Return a Read-only, adapted-to-unversioned, in-memory DAO that is
      // bootstrapped from a journal on disk.
      return this.ReadOnlyDAO.create({
        delegate: this.AdapterDAO.create({
          of: cls,
          to: versionedCls,
          delegate: this.JDAO.create({
            of: versionedCls,
            delegate: this.MDAO.create({of: versionedCls}),
            journal: this.NodeFileJournal.create({
              of: versionedCls,
              fd: this.getDataJournalFD_(versionedCls),
            }),
          }),
        }),
      });
    },
    function getDataJournalFD_(cls) {
      return require('fs').openSync(
          require('path').resolve(
              __dirname,
              `../data/${cls.id}-journal.js`),
          'r');
    },
  ],
});
