// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

require('angular');

require('../component/catalog_table.es6.js');
require('../component/api_analytics.es6.js');

angular.module('confluence')
  .controller('catalogController',
    ['$scope', 'api', function($scope, api) {
    // Activate dropdown and tabs.
    $('.add-browser-dropdown').dropdown();
    $('ul#view-tabs').tabs();
    let apiMatrix = api.matrix;
    $scope.showTab = 0;
    $scope.filteredViews = [];

    let alertError = function(errorMsg) {
      Materialize.toast(errorMsg, 4000);
    };

    apiMatrix.getBrowsers().then((browserGroups) => {
      $scope.browserGroups = browserGroups;
      $scope.expandBrowserDropdown = {};
      // Get latest browser keys for each browser as default browser set.
      let browsers = [];
      for (let browserName in browserGroups) {
        if (!browserGroups.hasOwnProperty(browserName)) continue;
        let versions = Object.keys($scope.browserGroups[browserName]);
        let latestVersion = versions.sort()[versions.length - 1];
        browsers.push(browserGroups[
          browserName][latestVersion][0]);
      }
      $scope.browsers = browsers;
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
      let browserKeys = $scope.browsers
        .map((browser) => browser.browserKey);
      if (browserKeys.indexOf(browserKey) >= 0) {
        alertError('This browser is already selected.');
        return;
      }
      // Array.push does not trigger Angular component's $onChanges listener.
      // Need to create a new Array.
      $scope.browsers = $scope.browsers.concat([browser]);
    };

    $scope.removeBrowser = function(browser) {
      let browserKey = browser.browserKey;
      let browserKeys = $scope.browsers
        .map((browser) => browser.browserKey);
      let removeIndex = browserKeys.indexOf(browserKey);
      if (removeIndex === -1) return;
      $scope.browsers.splice(removeIndex, 1);
      // Same as above, create a new array to trigger $onChanges listerner.
      $scope.browsers = $scope.browsers.slice();
    };

    $scope.createView = function(browser, option) {
      let browserOptions = {};
      switch(option) {
        case 'fallBehind':
          for (let i = 0; i < $scope.browsers.length; i++) {
            browserOptions[$scope.browsers[i].browserKey] = true;
          }
          browserOptions[browser.browserKey] = false;
          break;
        case 'proprietary':
          for (let i = 0; i < $scope.browsers.length; i++) {
            browserOptions[$scope.browsers[i].browserKey] = false;
          }
          browserOptions[browser.browserKey] = true;
          break;
      }
      $scope.filteredViews.push({
        browsers: $scope.browsers.slice(),
        browserOptions,
      });
    };
  }]);
