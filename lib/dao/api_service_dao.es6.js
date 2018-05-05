// Copyright 2018 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

require('./indexed_dao.es6.js');

foam.CLASS({
  name: 'ApiServiceDAO',
  package: 'org.chromium.apis.web',
  extends: 'foam.dao.ProxyDAO',

  documentation: `The top-level DAO for exposing DAOs in the AngularJS API
      service. This DAO is responsible for coordinating fetching and caching
      strategies to balance first data load / ongoing responsiveness
      tradeoffs.`,

  requires: [
    'foam.box.SkeletonBox',
    'foam.dao.ClientDAO',
    'foam.dao.RestDAO',
    'org.chromium.apis.web.WorkerDAO',
  ],

  properties: [
    {
      class: 'String',
      name: 'name',
      value: 'dao',
    },
    {
      class: 'String',
      name: 'baseURL',
      documentation: 'Base URL used for remote RestDAO.',
      required: true,
    },
    {
      class: 'FObjectProperty',
      of: 'foam.box.BoxRegistryBox',
      name: 'workerRegistry',
      documentation: `Remote foam.box.Box registry in worker that will host
          data cache.`,
      required: true,
    },
    {
      name: 'delegate',
      transient: true,
      factory: function() {
        this.validate();

        const clientDAO = this.ClientDAO.create({
          delegate: this.workerRegistry
              .register(this.name, null, this.SkeletonBox.create({
                data: this.WorkerDAO.create({
                  of: this.of,
                  baseURL: this.baseURL,
                }),
              })),
        });

        // Change delegate to client once it has finished caching in worker.
        clientDAO.limit(1).select().then(() => {
          this.delegate = clientDAO;
        });

        // Initially, hit network directly to fetch data.
        return this.RestDAO.create({
          of: this.of,
          baseURL: this.baseURL,
        });
      },
    },
  ],
});
