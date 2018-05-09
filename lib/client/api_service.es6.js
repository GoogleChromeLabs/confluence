// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

require('angular');

require('../dao/api_service_dao.es6.js');
require('../dao/dao_container.es6.js');
require('../web_apis/release.es6.js');
require('../web_apis/release_interface_relationship.es6.js');
require('../web_apis/web_interface.es6.js');
require('./api_confluence.es6.js');
require('./api_matrix.es6.js');
require('./events.es6.js');
require('./state.es6.js');
require('./stats_controller.es6.js');
const pkg = org.chromium.apis.web;

// TODO(markdittmer): Use foam.lookup() for class lookup. May also need to shove
// this into a FOAM class when porting to FOAM classloader.
angular.module('confluence').service('api', function() {
  const boxCtx = foam.box.Context.create({
    myname: '/page',
    unsafe: false,
    classWhitelist: require('../../data/class_whitelist.json'),
  });

  const stubFactory = foam.core.StubFactorySingleton.create();
  const Worker = require('../../main/worker.es6.js');

  const cachedDAONames = pkg.DAOContainer.CLIENT_CACHED_NAMES;
  let workers = [];
  let apiServiceDAOs = new Map();
  // Establish parallel URL structure by "name" parameter convention:
  //
  // Local FOAM box URL space:
  //
  //   /worker/{daoName} -- Box implementing DAO interface in worker
  //
  // Internet namesapace (DNS/HTTP service):
  //
  //   {location.origin}/{daoName} -- Basename for REST DAO endpoint
  const getApiServiceDAO = (name, cls, opt_ctx) => {
    const ctx = opt_ctx || boxCtx;

    // Return existing worker DAO when possible.
    if (apiServiceDAOs.has(name)) return apiServiceDAOs.get(name);

    // Use simple RestDAO for uncached DAOs.
    if (!cachedDAONames[name]) {
      const apiServiceDAO = foam.dao.RestDAO.create({
        of: cls,
        baseURL: `${location.origin}/${name}`,
      }, ctx);
      apiServiceDAOs.set(name, apiServiceDAO);
      return apiServiceDAO;
    }

    // Store Worker so it doesn't get garbage collected.
    const worker = new Worker();
    workers.push(worker);

    // First message to worker before foam.box.Context takes over:
    // "what is your name" message.
    const boxURL = `/worker/${name}`;
    worker.postMessage({name: boxURL});

    // Set up message passing with MessagePortBox.
    const workerMessagePortBox = foam.box.MessagePortBox.create({
      target: worker,
    }, ctx);

    // Bind worker messaging channel to its box URL.
    const namedBox = foam.box.NamedBox.create({
      name: boxURL,
      delegate: workerMessagePortBox,
    }, ctx);

    // Get a stub to the worker's service registry.
    const workerRegistry = stubFactory.get(foam.box.BoxRegistry).create({
      delegate: namedBox,
    }, ctx);

    // Create DAO responsible for registering its own caching strategy in
    // workerRegsitry, recieving its data from baseURL.
    const apiServiceDAO = pkg.ApiServiceDAO.create({
      of: cls,
      name,
      baseURL: `${location.origin}/${name}`,
      workerRegistry,
    }, ctx);

    // Store DAO in case it is requested again.
    apiServiceDAOs.set(name, apiServiceDAO);
    return apiServiceDAO;
  };

  const urlState = foam.web.URLState.create(null, boxCtx);

  // TODO(markdittmer): (Re)integrate stats controller into first render of
  // catalog views.
  const statsController =
      org.chromium.apis.web.StatsController.create(null, boxCtx);

  return {
    boxCtx,
    getApiServiceDAO,
    statsController,
    urlState,
    promises: [],
  };
});
