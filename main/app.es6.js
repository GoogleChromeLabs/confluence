// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

require('materialize-css/dist/js/materialize.js');
require('materialize-css/dist/css/materialize.css');
require('angular');
require('angular-ui-router');

require('../lib/web_apis/browser.es6.js');
require('../lib/web_apis/web_interface.es6.js');
require('../lib/web_apis/browser_interface_relationship.es6.js');
require('../lib/client/api_matrix.es6.js');

let app = angular.module('confluence', ['ui.router']);

require('../lib/controller/api_catalog.es6.js');

app.config(function($stateProvider, $urlRouterProvider) {
  let homeState = {
    name: 'home',
    url: '/',
    template: require('../static/view/home.html'),
  };

  // let confluenceState = {
  //   name: 'confluence',
  //   url: '/confluence',
  //   controller: 'confluenceController',
  //   template: require('../static/view/confluence.html'),
  // };

  let catalogState = {
    name: 'catalog',
    url: '/catalog',
    controller: 'catalogController',
    template: require('../static/view/api_catalog.html'),
  };

  $stateProvider.state(homeState);
  $stateProvider.state(catalogState);
  // $stateProvider.state(confluenceState);

  $urlRouterProvider.otherwise('/');
});

app.service('apiMatrix', function() {
  let browserApiDAO = foam.dao.RestDAO.create({
    baseURL: window.location.origin + '/browser-apis',
    of: org.chromium.apis.web.BrowserWebInterfaceJunction,
  });
  let browserDAO = foam.dao.RestDAO.create({
    baseURL: window.location.origin + '/browsers',
    of: org.chromium.apis.web.Browser,
  });
  let interfaceDAO = foam.dao.RestDAO.create({
    baseURL: window.location.origin + '/web-interfaces',
    of: org.chromium.apis.web.WebInterface,
  });
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
  return apiMatrix;
});
