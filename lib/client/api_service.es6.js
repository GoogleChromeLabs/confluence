// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

angular.module('confluence').service('api', ['$window', function($window) {
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
    baseURL: $window.location.origin + '/browser-apis',
    of: org.chromium.apis.web.BrowserWebInterfaceJunction,
  });
  let promises = [];
  promises.push(foam.dao.RestDAO.create({
    baseURL: $window.location.origin + '/browsers',
    of: org.chromium.apis.web.Browser,
  }).select({
    put: (browser) => {
      browserDAO.put(browser);
    },
  }));

  promises.push(foam.dao.RestDAO.create({
    baseURL: window.location.origin + '/web-interfaces',
    of: org.chromium.apis.web.WebInterface,
  }).select({
    put: (webInterface) => {
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
  return {
    matrix: apiMatrix,
    promises,
  };
}]);
