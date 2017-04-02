// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

function analyticsController($scope) {
  let ctrl = this;
  ctrl.$onInit = function() {
      // $scope.analytics = ctrl.apiMatrix.getAnalytics(ctrl.browserKeys);
  };
  ctrl.$onChanges = function(changes) {
    if (changes.browserKeys) {
      // ctrl.apiMatrix.getAnalytics(changes.browserKeys.currentValue)
      // .then((analytics) => {
      //   $scope.analytics = analytics;
      //   $scope.$apply();
      // });
    }
  };
  $scope.createNewView = function(browserKey, option) {
    ctrl.onCreateNewView({browserKey, option});
  };
}

angular.module('confluence').component('apiAnalytics', {
  template: require('../../static/component/api_analytics.html'),
  controller: analyticsController,
  bindings: {
    apiMatrix: '<',
    browserKeys: '<',
    onCreateNewView: '&',
  },
});
