// Copyright 2018 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

require('./indexed_dao.es6.js');
require('./dao_factory.es6.js');

foam.CLASS({
  name: 'WorkerDAO',
  package: 'org.chromium.apis.web',
  extends: 'org.chromium.apis.web.SerializableIndexedDAO',

  documentation: `A serializable DAO wrapper around foam.dao.CachingDAO. Clients
      of this DAO rely on the following properties of foam.dao.CachingDAO:

      1. Stores a complete copy of remote data;
      2. Does not service DAO operations until local copy is fully
         initialized.`,

  requires: [
    'foam.dao.CachingDAO',
    'foam.dao.MDAO',
    'foam.dao.RestDAO',
    'org.chromium.apis.web.RestDAOFactory',
  ],
  imports: ['container'],

  properties: [
    {
      class: 'String',
      name: 'baseURL',
      documentation: 'Base URL used for remote RestDAO to cache.',
      required: true,
    },
    {
      name: 'delegate',
      transient: true,
      factory: function() {
        this.validate();

        const CachingDAO = this.localCtx.lookup('foam.dao.CachingDAO');
        const MDAO = this.localCtx.lookup('foam.dao.MDAO');
        const RestDAO = this.localCtx.lookup('foam.dao.RestDAO');
        const cache = MDAO.create({
          of: this.of,
        }, this.localCtx);
        this.propertyIndexGroups
            .forEach(index => cache.addPropertyIndex.apply(cache, index));
        return CachingDAO.create({
          of: this.of,
          src: RestDAO.create({
            of: this.of,
            baseURL: this.baseURL,
          }, this.localCtx),
          cache,
        }, this.localCtx);
      },
    },
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
});
