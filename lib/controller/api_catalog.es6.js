// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

require('angular');

require('../component/catalog_table.es6.js');
const pkg = org.chromium.apis.web;

angular.module('confluence')
  .controller('catalogController',
    ['$scope', 'api', function($scope, api) {
    // Activate dropdown and tabs.
    $('.add-release-dropdown').dropdown();
    $('ul#view-tabs').tabs();
    const matrixController = api.apiMatrixController;
    $scope.showTab = 0;
    $scope.filteredViews = [];

    let alertError = function(errorMsg) {
      Materialize.toast(errorMsg, 4000);
    };

    api.bindState($scope, 'releaseKeys', []);

    $scope.releases = [];
    $scope.releaseGroups = {};
    $scope.expandReleaseDropdown = {};

    // Keep scope's "releases" and "releaseKeys" in sync.
    function setReleases(releases) {
      $scope.releases = releases;
      $scope.releaseKeys = releases.map(release => release.releaseKey);
    }

    Promise.all([
      // Use either non-empty releaseKeys array from URL state,
      // or latest releases.
      ($scope.releaseKeys.length > 0 ?
       matrixController.setReleaseKeys($scope.releaseKeys) :
       matrixController.setLatestReleases())
          .then(releases => setReleases(releases)),
      matrixController.getReleaseGroups().then(gs => $scope.releaseGroups = gs),
    ]).then(() => $scope.$apply());

    $scope.expandReleaseList = function($event, release, version) {
      // Stop propagation to stop dropdown list disapearing.
      $event.stopPropagation();
      if (version) {
        if ($scope.expandReleaseDropdown[release].hasOwnProperty(version)) {
          delete $scope.expandReleaseDropdown[release][version];
        } else {
          $scope.expandReleaseDropdown[release][version] = true;
        }
        return;
      }
      if (release) {
        if ($scope.expandReleaseDropdown.hasOwnProperty(release)) {
          delete $scope.expandReleaseDropdown[release];
        } else {
          $scope.expandReleaseDropdown[release] = {};
        }
      }
    };

    $scope.addRelease = function(release) {
      let releaseKey = release.releaseKey;
      let releaseKeys = $scope.releaseKeys;

      if (releaseKeys.indexOf(releaseKey) >= 0) {
        alertError('This release is already selected.');
        return;
      }

      // Array.push does not trigger Angular component's $onChanges listener.
      // Need to create a new Array.
      setReleases($scope.releases.concat([release]));
    };

    $scope.removeRelease = function(release) {
      let releaseKey = release.releaseKey;
      let releaseKeys = $scope.releaseKeys;
      let removeIndex = releaseKeys.indexOf(releaseKey);
      if (removeIndex === -1) return;

      $scope.releases.splice(removeIndex, 1);
      // Same as above, create a new array to trigger $onChanges listener.
      setReleases($scope.releases.slice());
    };
  }]);
