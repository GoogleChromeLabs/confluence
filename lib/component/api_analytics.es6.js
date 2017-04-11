// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

function analyticsController($scope, api) {
  let ctrl = this;
  let apiMatrix = api.matrix;
  ctrl.$onChanges = function(changes) {
    if (changes.browsers && changes.browsers.currentValue) {
      let browserKeys = changes.browsers.currentValue.map(
          (browser) => browser.browserKey);
      apiMatrix.getAnalytics(browserKeys).then((analytics) => {
        $scope.analytics = analytics;
        $scope.$apply();
      });
    }
  };
  $scope.createNewView = function(browserKey, option) {
    ctrl.onCreateNewView({browserKey, option});
  };
}

angular.module('confluence').component('apiAnalytics', {
  template: require('../../static/component/api_analytics.html'),
  controller: ['$scope', 'api', analyticsController],
  bindings: {
    browsers: '<',
    onCreateNewView: '&',
  },
});
