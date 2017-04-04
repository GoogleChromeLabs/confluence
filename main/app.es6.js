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

require('../lib/client/api_service.es6.js');
require('../lib/controller/api_catalog.es6.js');

app.config(function($stateProvider, $urlRouterProvider) {
  $stateProvider.state({
    name: 'home',
    url: '/',
    template: require('../static/view/home.html'),
  });

  $stateProvider.state({
    name: 'catalog',
    url: '/catalog',
    controller: 'catalogController',
    template: require('../static/view/api_catalog.html'),
    resolve: {
      apiPromises: function(api) {
        return Promise.all(api.promises);
      },
    },
  });

  $urlRouterProvider.otherwise('/');
});

