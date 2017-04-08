// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

require('../chart/api_velocity_chart.es6');
require('../chart/failure_to_ship_chart.es6');
require('../chart/vendor_specific_chart.es6');
// require('./aggressive_removal_chart.es6');

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

    // apiConfluence.getAggressiveRemoval()
    //   .then((aggressiveRemovalMetrics) => {
    //     $scope.aggressiveRemovalMetrics = aggressiveRemovalMetrics;
    //   });

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
    // $scope.newRemovedView = function(browserKey, prevBrowserKey,
    //   comparedBrowserKeys) {
    //     let browserKeys = comparedBrowserKeys.slice();
    //     browserKeys.push(browserKey, prevBrowserKey);
    //     let browserOptions = {};
    //     // Set browser options to be true for all compared
    //     // browser keys and previous browser key, false for
    //     // current brwoserkey. This will filter out
    //     // the remvoed APIs from current version which
    //     // still exists in other browsers.
    //     browserOptions[prevBrowserKey] = true;
    //     browserOptions[browserKey] = false;
    //     let lengths = [];
    //     for (let i = 2; i < browserKeys.length; i++) {
    //       lengths.push(i);
    //     }
    //     $scope.additionalViews.push({
    //       browserKeys,
    //       browserOptions,
    //       lengths,
    //     });
    // };
  }]);
