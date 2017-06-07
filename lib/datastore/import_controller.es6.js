// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

foam.CLASS({
  name: 'ImportController',
  package: 'org.chromium.apis.web',

  documentation: 'Controller acting as container for datastore import context',

  requires: [
    'com.google.cloud.datastore.BatchMutationDatastoreDAO',
    'com.google.net.node.Google2LOAuthAgent',
    'foam.nanos.log.ConsoleLogger',
    'org.chromium.apis.web.BrowserMetricData',
    'org.chromium.apis.web.Release',
    'org.chromium.apis.web.ReleaseWebInterfaceJunction',
    'org.chromium.apis.web.WebInterface',
  ],
  exports: [
    'browserMetricsDAO',
    'gcloudProjectId',
    'releaseDAO',
    'releaseWebInterfaceJunctionDAO',
    'webInterfaceDAO',
  ],

  properties: [
    {
      class: 'String',
      documentation: `Email account used to authenticate against Google Cloud
          services.`,
      name: 'gcloudAuthEmail',
      required: true
    },
    {
      class: 'String',
      documentation: `Private key used to authenticate against Google Cloud
          services.`,
      name: 'gcloudAuthPrivateKey',
      required: true
    },
    {
      class: 'String',
      documentation: 'Google Cloud Project ID for Datastore deployment.',
      name: 'gcloudProjectId',
      value: 'web-confluence',
    },
    {
      name: 'ctx',
      documentation: 'Context in which components are created',
      factory: function() {
        // Cascade exports from logger and authAgent into context.
        var ctx = this.__subContext__.createSubContext(this.logger);
        if (!this.authAgent) return ctx;

        ctx = ctx.createSubContext(this.authAgent);

        // Inherit HTTPRequest override from authAgent.
        //
        // TODO(markdittmer): This should be unnecessary: Context mixins
        // should pull in class registration overrides, but it currently
        // doesn't.
        ctx.register(
            this.authAgent.__subContext__.lookup('foam.net.HTTPRequest'),
            'foam.net.HTTPRequest');
        return ctx;
      },
    },
    {
      class: 'FObjectProperty',
      of: 'foam.nanos.log.Logger',
      name: 'logger',
      factory: function() { return this.ConsoleLogger.create(); },
    },
    {
      class: 'FObjectProperty',
      of: 'foam.net.auth.AuthAgent',
      name: 'authAgent',
      factory: function() {
        return this.Google2LOAuthAgent.create({
          requiresAuthorization: function(request) {
            if (request.url) request = request.fromUrl(request.url);
            return request.protocol === 'https' &&
                request.hostname === 'datastore.googleapis.com';
          },
          email: this.gcloudAuthEmail,
          privateKey: this.gcloudAuthPrivateKey,
          scopes: [
            'https://www.googleapis.com/auth/cloud-platform',
            'https://www.googleapis.com/auth/datastore'
          ],
        });
      },
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
    {
      class: 'foam.dao.DAOProperty',
      documentation: `DAO providing access to historical browser metrics data.`,
      name: 'browserMetricsDAO',
      factory: function() {
        return this.BatchMutationDatastoreDAO.create({
          of: this.BrowserMetricData,
        }, this.ctx);
      },
    },
  ],
});
