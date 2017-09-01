// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

require('angular');

require('../dao_container.es6.js');
require('../web_apis/release.es6.js');
require('../web_apis/release_interface_relationship.es6.js');
require('../web_apis/web_interface.es6.js');
require('./api_confluence.es6.js');
require('./api_matrix.es6.js');
require('./events.es6.js');
const pkg = org.chromium.apis.web;

// TODO(markdittmer): Use foam.lookup() for class lookup. May also need to shove
// this into a FOAM class when porting to FOAM classloader.
angular.module('confluence').service('api', ['$window', function($window) {
  const ctx = foam.box.Context.create({
    myname: '/page',
  });
  const registry = ctx.registry;
  const workerEvents = foam.core.FObject.create();
  const workerEventsBox = foam.box.EventDispatchBox.create({
    target: workerEvents,
  }, ctx);
  registry.register('events', null, workerEventsBox);

  const workerMessagePortBox = foam.box.MessagePortBox.create({
      target: new Worker('sync_worker.bundle.js'),
  }, ctx);
  workerMessagePortBox.delegate;
  foam.box.NamedBox.create({
    name: '/sync_worker',
    delegate: workerMessagePortBox,
  }, ctx);



  const C = pkg.DAOContainer;
  // Names must match those registered in in server box context.
  function getClientDAO(name, cls) {
    return foam.dao.ClientDAO.create({
      of: cls,
      delegate: foam.box.NamedBox.create({
        name: `/sync_worker/${name}`,
      }, ctx),
    }, ctx);
  }

  let releaseDAO = getClientDAO(C.RELEASE_NAME, pkg.Release);
  let webInterfaceDAO = getClientDAO(C.WEB_INTERFACE_NAME, pkg.WebInterface);
  let releaseWebInterfaceJunctionDAO = getClientDAO(
      C.RELEASE_WEB_INTERFACE_JUNCTION_NAME,
      pkg.ReleaseWebInterfaceJunction);
  let apiMatrix = org.chromium.apis.web.ApiMatrix.create({
    releaseWebInterfaceJunctionDAO,
    releaseDAO,
    webInterfaceDAO,
  },
  // Provide a context that is aware to relationship DAOs.
  // TODO(markdittmer): providing an interface for binding
  // DAOs on Relationships.
  foam.__context__.createSubContext({
    releaseDAO,
    webInterfaceDAO,
    releaseWebInterfaceJunctionDAO,
  }));

  let apiVelocityDAO =
      getClientDAO(C.API_VELOCITY_NAME, pkg.ApiVelocityData);
  let failureToShipDAO =
      getClientDAO(C.FAILURE_TO_SHIP_NAME, pkg.BrowserMetricData);
  let browserSpecificDAO =
      getClientDAO(C.BROWSER_SPECIFIC_NAME, pkg.BrowserMetricData);
  let aggressiveRemovalDAO =
      getClientDAO(C.AGGRESSIVE_REMOVAL_NAME, pkg.BrowserMetricData);
  let apiConfluence = pkg.ApiConfluence.create({
    apiVelocityDAO,
    failureToShipDAO,
    browserSpecificDAO,
    aggressiveRemovalDAO,
  });

  const E = foam.mlang.ExpressionsSingleton.create();
  const latestReleases = pkg.events.waitUntil(workerEvents, [
    pkg.DAOSyncedEvent.create({name: C.RELEASE_NAME}),
  ]).then(() => apiMatrix.SEPARATE_LATEST_RELEASES(releaseDAO))
        .then(latestAndRest => latestAndRest.latest);
  const apiDAO = pkg.events.waitUntil(workerEvents, [
    pkg.DAOSyncedEvent.create({name: C.WEB_INTERFACE_NAME}),
  ]).then(() => webInterfaceDAO);
  const latestAPIData = latestReleases.then(releases => pkg.events.waitUntil(
      workerEvents, releases.map(release => pkg.DAOSyncedEvent.create({
          name: C.RELEASE_WEB_INTERFACE_JUNCTION_NAME,
          predicate: E.EQ(
              // TODO(markdittmer): This main-page component should not need to
              // know about worker's versioned class.
              pkg.VersionedReleaseWebInterfaceJunction.SOURCE_ID,
              release.id),
        }))));

  return {
    matrix: apiMatrix,
    confluence: apiConfluence,
    workerEvents: workerEvents,
    latestReleases: latestReleases,
    apiDAO: apiDAO,
    latestAPIData: latestAPIData,
    promises: [],
  };
}]);
