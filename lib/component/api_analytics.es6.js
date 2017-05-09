// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

require('angular');

function analyticsController($scope, api) {
  let ctrl = this;
  let apiMatrix = api.matrix;
  ctrl.$onChanges = function(changes) {
    if (changes.releases && changes.releases.currentValue) {
      let releaseKeys = changes.releases.currentValue.map(
          (release) => release.releaseKey);
      apiMatrix.getAnalytics(releaseKeys).then((analytics) => {
        $scope.analytics = analytics;
        $scope.$apply();
      });
    }
  };
  $scope.createNewView = function(releaseKey, option) {
    ctrl.onCreateNewView({releaseKey, option});
  };
}

angular.module('confluence').component('apiAnalytics', {
  template: require('../../static/component/api_analytics.html'),
  controller: ['$scope', 'api', analyticsController],
  bindings: {
    releases: '<',
    onCreateNewView: '&',
  },
});
