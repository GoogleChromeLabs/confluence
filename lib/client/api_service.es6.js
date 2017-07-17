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

// TODO(markdittmer): Use foam.lookup() for class lookup. May also need to shove
// this into a FOAM class when porting to FOAM classloader.
angular.module('confluence').service('api', ['$window', function($window) {
  const ORIGIN = $window.location.origin;

  const ctx = foam.box.Context.create();
  const C = org.chromium.apis.web.DAOContainer;
  // Names must match those registered in in server box context.
  function getClientDAO(name, cls) {
    return foam.dao.StreamingClientDAO.create({
      of: cls,
      delegate: foam.box.SubBox.create({
        name: name,
        delegate: foam.box.WebSocketBox.create({
          // TODO(markdittmer): Explicitly select webSocketService port.
          // (Current default: 4000.)
          uri: 'ws://0.0.0.0:4000',
        }, ctx),
      }, ctx),
    }, ctx);
  }

  const Release = org.chromium.apis.web.Release;
  const WebInterface = org.chromium.apis.web.WebInterface;
  const ReleaseWebInterfaceJunction =
      org.chromium.apis.web.ReleaseWebInterfaceJunction;

  let releaseDAO = foam.dao.EasyDAO.create({
    name: C.RELEASE_NAME,
    of: Release,
    daoType: 'MDAO',
  });
  let webInterfaceDAO = foam.dao.EasyDAO.create({
    name: C.WEB_INTERFACE_NAME,
    of: WebInterface,
    daoType: 'MDAO',
  });
  let releaseWebInterfaceJunctionDAO = getClientDAO(
      C.RELEASE_WEB_INTERFACE_JUNCTION_NAME,
      ReleaseWebInterfaceJunction);
  let promises = [];
  promises.push(getClientDAO(C.RELEASE_NAME, Release)
      .select(foam.dao.AnonymousSink.create({
        sink: {put: release => releaseDAO.put(release)},
      })));
  promises.push(getClientDAO(C.WEB_INTERFACE_NAME, WebInterface)
      .select(foam.dao.AnonymousSink.create({
        sink: {put: webInterface => webInterfaceDAO.put(webInterface)},
      })));
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

  const ApiVelocityData = org.chromium.apis.web.ApiVelocityData;
  const BrowserMetricData = org.chromium.apis.web.BrowserMetricData;
  let apiVelocityDAO =
      getClientDAO(C.API_VELOCITY_NAME, ApiVelocityData);
  let failureToShipDAO =
      getClientDAO(C.FAILURE_TO_SHIP_NAME, BrowserMetricData);
  let browserSpecificDAO =
      getClientDAO(C.BROWSER_SPECIFIC_NAME, BrowserMetricData);
  let aggressiveRemovalDAO =
      getClientDAO(C.AGGRESSIVE_REMOVAL_NAME, BrowserMetricData);
  let apiConfluence = org.chromium.apis.web.ApiConfluence.create({
    apiVelocityDAO,
    failureToShipDAO,
    browserSpecificDAO,
    aggressiveRemovalDAO,
  });

  return {
    matrix: apiMatrix,
    confluence: apiConfluence,
    promises,
  };
}]);
