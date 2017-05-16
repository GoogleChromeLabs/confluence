// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

foam.CLASS({
  name: 'ImportController',
  package: 'org.chromium.apis.web',

  documentation: 'Controller acting as container for datastore import context',

  requires: [
    'com.google.cloud.datastore.BatchMutationDatastoreDAO',
    'foam.nanos.log.ConsoleLogger',
    'org.chromium.apis.web.DatastoreImporter',
    'org.chromium.apis.web.Release',
    'org.chromium.apis.web.ReleaseWebInterfaceJunction',
    'org.chromium.apis.web.WebInterface',
  ],
  exports: [
    'gcloudProjectId',
    'releaseDAO',
    'releaseWebInterfaceJunctionDAO',
    'webInterfaceDAO',
  ],

  properties: [
    {
      name: 'ctx',
      documentation: 'Context in which components are created',
      factory: function() {
        return this.__subContext__.createSubContext(this.logger);
      },
    },
    {
      class: 'FObjectProperty',
      of: 'foam.nanos.log.Logger',
      name: 'logger',
      factory: function() { return this.ConsoleLogger.create(); },
    },
    {
      class: 'String',
      documentation: 'Google Cloud Project ID for Datastore deployment.',
      name: 'gcloudProjectId',
      value: 'web-confluence',
    },
    {
      class: 'foam.dao.DAOProperty',
      documentation: 'DAO providing access to all known browser releases.',
      name: 'releaseDAO',
      factory: function() {
        return this.BatchMutationDatastoreDAO.create({
          of: this.Release,
        }, this.ctx);
      },
    },
    {
      class: 'foam.dao.DAOProperty',
      documentation: 'DAO providing access to all known web APIs.',
      name: 'webInterfaceDAO',
      factory: function() {
        return this.BatchMutationDatastoreDAO.create({
          of: this.WebInterface,
        }, this.ctx);
      },
    },
    {
      class: 'foam.dao.DAOProperty',
      documentation: `DAO providing access to all browser release <--> API
          relations.`,
      name: 'releaseWebInterfaceJunctionDAO',
      factory: function() {
        return this.BatchMutationDatastoreDAO.create({
          of: this.ReleaseWebInterfaceJunction,
        }, this.ctx);
      },
    },
  ],
});
