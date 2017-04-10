// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

require('../chart/api_velocity_chart.es6.js');
require('../chart/failure_to_ship_chart.es6.js');
require('../chart/vendor_specific_chart.es6.js');
require('../chart/aggressive_removal_chart.es6.js');

angular.module('confluence')
  .controller('confluenceController', ['$scope', 'api', function($scope, api) {
    // Activate tabs.
    $('ul.tabs').tabs();
    $scope.showTab = 0;
    $scope.apiVelocityMetrics = {};
    $scope.failureToShipMetric = null;
    $scope.vendorSpecificMetric = null;
    $scope.additionalViews = [];
    $scope.apiMatrix = api.matrix;
    let apiConfluence = api.confluence;

    apiConfluence.getApiVelocity().then((apiVelocityMetrics) => {
      $scope.apiVelocityMetrics = apiVelocityMetrics;
      $scope.$apply();
    });

    apiConfluence.getFailureToShip().then((failureToShipMetric) => {
      $scope.failureToShipMetric = failureToShipMetric;
      $scope.$apply();
    });

    apiConfluence.getVendorSpecificApis().then((vendorSpecificMetric) => {
      $scope.vendorSpecificMetric = vendorSpecificMetric;
      $scope.$apply();
    });

    apiConfluence.getAggressiveRemoval()
      .then((aggressiveRemovalMetrics) => {
        $scope.aggressiveRemovalMetrics = aggressiveRemovalMetrics;
      });

    $scope.createNewDiffView = function(minuend, subtrahend) {
      let browserOptions = {};
      browserOptions[minuend.browserKey] = true;
      browserOptions[subtrahend.browserKey] = false;
      $scope.additionalViews.push({
        browsers: [minuend, subtrahend],
        browserOptions,
      });
    };
    $scope.newFailureToShipView = function(browser, prevs, others) {
      let browserOptions = {};
      let browsers = [browser].concat(prevs, others);
      browserOptions[browser.browserKey] = false;
      for (let i = 0; i < prevs.length; i++) {
        browserOptions[prevs[i].browserKey] = false;
      }
      for (let i = 0; i < others.length; i++) {
        browserOptions[others[i].browserKey] = true;
      }
      $scope.additionalViews.push({
        browsers,
        browserOptions,
      });
    };
    $scope.newVendorSpecificView = function(browser, prevs, others) {
      let browserOptions = {};
      let browsers = [browser].concat(prevs, others);
      browserOptions[browser.browserKey] = true;
      for (let i = 0; i < prevs.length; i++) {
        browserOptions[prevs[i].browserKey] = true;
      }
      for (let i = 0; i < others.length; i++) {
        browserOptions[others[i].browserKey] = false;
      }
      $scope.additionalViews.push({
        browsers,
        browserOptions,
      });
    };
    $scope.newAggressiveRemovalView = function(browserTwoYearsAgo,
      prevReleaseBrowsers, currBrowsers) {
      console.log(browserTwoYearsAgo, prevReleaseBrowsers, currBrowsers)
        let browsers = [browserTwoYearsAgo]
          .concat(prevReleaseBrowsers, currBrowsers);
        let browserOptions = {};
        // Set browser options to be true for browserTwoYearsAgo and
        // all currBrowsers. And set numAvailable to be 1 +
        // currBrowsers.length to browsers.length. So it returns
        // APIs that not exists in browserTwoYearsAgo but avaiable
        // in all current Browsers and some prevReleaseBrowsers;
        browserOptions[browserTwoYearsAgo.browserKey] = false;
        for (let i = 0; i < currBrowsers.length; i++) {
          browserOptions[currBrowsers[i].browserKey] = true;
        }
        let numAvailable = [];
        for (let i = currBrowsers.length + 1; i <= browsers.length; i++) {
          numAvailable.push(i);
        }
        console.log(numAvailable);
        $scope.additionalViews.push({
          browsers,
          browserOptions,
          numAvailable,
        });
    };
  }]);
