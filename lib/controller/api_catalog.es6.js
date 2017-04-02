// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

require('../component/catalog_table.es6.js');
require('../component/api_analytics.es6.js');

angular.module('confluence')
  .controller('catalogController',
    ['$scope', 'apiMatrix', function($scope, apiMatrix) {
    // Activate dropdown and tabs.
    $('.add-browser-dropdown').dropdown();
    $('ul#view-tabs').tabs();
    $scope.apiMatrix = apiMatrix;
    $scope.showTab = 0;
    $scope.filteredViews = [];

    function alertError(errorMsg) {
      Materialize.toast(errorMsg, 4000);
    }

    apiMatrix.getBrowsers()
    .then((browserGroups) => {
      $scope.browserGroups = browserGroups;
      $scope.expandBrowserDropdown = {};
      // Get latest browser keys.
      $scope.browserKeys = [];
      for (let browserName in browserGroups) {
        if (!browserGroups.hasOwnProperty(browserName)) continue;
        let versions = Object.keys($scope.browserGroups[browserName]);
        let latestVersion = versions.sort()[versions.length - 1];
        $scope.browserKeys.push(browserGroups[
          browserName][latestVersion][0].browserKey);
      }
      $scope.$apply();
    });

    $scope.expandBrowserList = function($event, browser, version) {
      // Stop propagation to stop dropdown list disapearing.
      $event.stopPropagation();
      if (version) {
        if ($scope.expandBrowserDropdown[browser].hasOwnProperty(version)) {
          delete $scope.expandBrowserDropdown[browser][version];
        } else {
          $scope.expandBrowserDropdown[browser][version] = true;
        }
        return;
      }
      if (browser) {
        if ($scope.expandBrowserDropdown.hasOwnProperty(browser)) {
          delete $scope.expandBrowserDropdown[browser];
        } else {
          $scope.expandBrowserDropdown[browser] = {};
        }
      }
    };

    $scope.addBrowser = function(browser) {
      let browserKey = browser.browserKey;
      if ($scope.browserKeys.indexOf(browserKey) >= 0) {
        alertError('This browser is already selected.');
        return;
      }
      // Array.push does not trigger component's $onChanges listener.
      // Need to create a new Array.
      $scope.browserKeys = $scope.browserKeys.concat([browserKey]);
    };

    $scope.removeBrowser = function(browserKey) {
      let removeIndex = $scope.browserKeys.indexOf(browserKey);
      if (removeIndex === -1) return;
      $scope.browserKeys.splice(removeIndex, 1);
      // Same as above, create a new array to trigger $onChanges listerner.
      $scope.browserKeys = $scope.browserKeys.slice();
    };

    $scope.createView = function(browserKey, option) {
      let browserOptions = {};
      switch(option) {
        case 'fallBehind':
          for (let i = 0; i < $scope.browserKeys.length; i++) {
            browserOptions[$scope.browserKeys[i]] = true;
          }
          browserOptions[browserKey] = false;
          break;
        case 'proprietary':
          for (let i = 0; i < $scope.browserKeys.length; i++) {
            browserOptions[$scope.browserKeys[i]] = false;
          }
          browserOptions[browserKey] = true;
          break;
      }
      $scope.filteredViews.push({
        browserKeys: $scope.browserKeys.slice(),
        browserOptions,
      });
    };
  }]);
