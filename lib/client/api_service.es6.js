// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

require('angular');

require('./api_confluence.es6.js');
require('./api_matrix.es6.js');
require('../web_apis/release.es6.js');
require('../web_apis/release_interface_relationship.es6.js');
require('../web_apis/web_interface.es6.js');

// TODO(markdittmer): Use foam.lookup() for class lookup. May also need to shove
// this into a FOAM class when porting to FOAM classloader.
angular.module('confluence').service('api', ['$window', function($window) {
  const ORIGIN = $window.location.origin;
  const RELEASE_API_URL = ORIGIN + '/release-apis';
  const RELEASE_URL = ORIGIN + '/releases';
  const WEB_INTERFACE_URL = ORIGIN + '/web-interfaces';
  const API_VELOCITY_URL = ORIGIN + '/api-velocity';
  const FAILURE_TO_SHIP_URL = ORIGIN + '/failure-to-ship';
  const BROWSER_SPECIFIC_URL = ORIGIN + '/browser-specific';
  const AGGRESSIVE_REMOVAL_URL = ORIGIN + '/aggressive-removal';

  let releaseDAO = foam.dao.EasyDAO.create({
    name: 'releaseDAO',
    of: org.chromium.apis.web.Release,
    daoType: 'MDAO',
  });
  let webInterfaceDAO = foam.dao.EasyDAO.create({
    name: 'webInterfaceDAO',
    of: org.chromium.apis.web.WebInterface,
    daoType: 'MDAO',
  });
  let releaseWebInterfaceJunctionDAO = foam.dao.RestDAO.create({
    baseURL: RELEASE_API_URL,
    of: org.chromium.apis.web.ReleaseWebInterfaceJunction,
  });
  let promises = [];
  promises.push(foam.dao.RestDAO.create({
    baseURL: RELEASE_URL,
    of: org.chromium.apis.web.Release,
  }).select(foam.dao.AnonymousSink.create({
    sink: {put: (_, release) => {
      releaseDAO.put(release);
    }},
  })));
  promises.push(foam.dao.RestDAO.create({
    baseURL: WEB_INTERFACE_URL,
    of: org.chromium.apis.web.WebInterface,
  }).select(foam.dao.AnonymousSink.create({
    sink: {put: (_, webInterface) => {
      webInterfaceDAO.put(webInterface);
    }},
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
    webInterfaceDAO: webInterfaceDAO,
    releaseWebInterfaceJunctionDAO: releaseWebInterfaceJunctionDAO,
  }));

  let apiVelocityDAO = foam.dao.RestDAO.create({
    baseURL: API_VELOCITY_URL,
  });
  let failureToShipDAO = foam.dao.RestDAO.create({
    baseURL: FAILURE_TO_SHIP_URL,
  });
  let browserSpecificDAO = foam.dao.RestDAO.create({
    baseURL: BROWSER_SPECIFIC_URL,
  });
  let aggressiveRemovalDAO = foam.dao.RestDAO.create({
    baseURL: AGGRESSIVE_REMOVAL_URL,
  });
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
