// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

require('angular');

angular.module('confluence').service('api', ['$window', function($window) {
  const ORIGIN = $window.location.origin;
  const BROWSER_API_URL = ORIGIN + '/browser-apis';
  const BROWSER_URL = ORIGIN + '/browsers';
  const WEB_INTERFACE_URL = ORIGIN + '/web-interfaces';
  const API_VELOCITY_URL = ORIGIN + '/api-velocity';
  const FAILURE_TO_SHIP_URL = ORIGIN + '/failure-to-ship';
  const VENDOR_SPECIFIC_URL = ORIGIN + '/vendor-specific';
  const AGGRESSIVE_REMOVAL_URL = ORIGIN + '/aggressive-removal';

  let browserDAO = foam.dao.EasyDAO.create({
    name: 'browserDAO',
    of: org.chromium.apis.web.Browser,
    daoType: 'MDAO',
  });
  let interfaceDAO = foam.dao.EasyDAO.create({
    name: 'interfaceDAO',
    of: org.chromium.apis.web.WebInterface,
    daoType: 'MDAO',
  });
  let browserApiDAO = foam.dao.RestDAO.create({
    baseURL: BROWSER_API_URL,
    of: org.chromium.apis.web.BrowserWebInterfaceJunction,
  });
  let promises = [];
  promises.push(foam.dao.RestDAO.create({
    baseURL: BROWSER_URL,
    of: org.chromium.apis.web.Browser,
  }).select({
    put: (_, browser) => {
      browserDAO.put(browser);
    },
  }));
  promises.push(foam.dao.RestDAO.create({
    baseURL: WEB_INTERFACE_URL,
    of: org.chromium.apis.web.WebInterface,
  }).select({
    put: (_, webInterface) => {
      interfaceDAO.put(webInterface);
    },
  }));
  let apiMatrix = org.chromium.apis.web.ApiMatrix.create({
    browserApiDAO,
    browserDAO,
    interfaceDAO,
  },
  // Provide a context that is aware to relationship DAOs.
  // TODO(markdittmer): providing an interface for binding
  // DAOs on Relationships.
  foam.__context__.createSubContext({
    browserDAO,
    webInterfaceDAO: interfaceDAO,
    browserWebInterfaceJunctionDAO: browserApiDAO,
  }));

  let apiVelocityDAO = foam.dao.RestDAO.create({
    baseURL: API_VELOCITY_URL,
  });
  let failureToShipDAO = foam.dao.RestDAO.create({
    baseURL: FAILURE_TO_SHIP_URL,
  });
  let vendorSpecificDAO = foam.dao.RestDAO.create({
    baseURL: VENDOR_SPECIFIC_URL,
  });
  let aggressiveRemovalDAO = foam.dao.RestDAO.create({
    baseURL: AGGRESSIVE_REMOVAL_URL,
  });
  let apiConfluence = org.chromium.apis.web.ApiConfluence.create({
    apiVelocityDAO,
    failureToShipDAO,
    vendorSpecificDAO,
    aggressiveRemovalDAO,
  });

  return {
    matrix: apiMatrix,
    confluence: apiConfluence,
    promises,
  };
}]);
