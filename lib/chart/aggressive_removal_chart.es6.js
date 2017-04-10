// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

let d3 = require('d3');

function aggressiveRemovalController($scope, $timeout) {
  let createNewView = function(browserOneYearAgo,
    prevReleaseBrowsers, currBrowsers) {
      this.createNewView({browserOneYearAgo, prevReleaseBrowsers,
        currBrowsers});
    }.bind(this);

  this.$onInit = function() {
    // AngularJS does not seem to have reliable post-render callbacks.
    // Use $timeout as a hack.
    $timeout(function() {
      org.chromium.apis.web.TimeSeriesChart.create({
        $scope,
        title: this.title,
        chartId: this.chartId,
        metric: this.aggressiveRemovalMetrics,
        valueName: 'numAggressiveRemoval',
        tooltipGen: function(d) {
          return {
            browserName: d.browserOneYearAgo.browserName,
            properties: [
              {
                name: 'version 1 year ago',
                value: d.browserOneYearAgo.browserVersion,
              },
              {
                name: 'Aggressive Removal',
                value: d.numAggressiveRemoval,
                action: function() {
                  createNewView(d.browserOneYearAgo,
                    d.prevReleaseBrowsers,
                    d.currBrowsers);
                },
              },
            ],
          };
        },
      });
    }.bind(this));
  };
}

angular.module('confluence').component('aggressiveRemovalChart', {
  template: require('../../static/component/chart.html'),
  controller: ['$scope', '$timeout', aggressiveRemovalController],
  bindings: {
    aggressiveRemovalMetrics: '<',
    title: '@',
    chartId: '@',
    createNewView: '&',
  },
});
