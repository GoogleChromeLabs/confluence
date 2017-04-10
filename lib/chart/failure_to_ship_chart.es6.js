// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

function failureToShipChartController($scope, $timeout) {
  let createNewView = function(browser, prevBrowsers, comparedBrowsers) {
    this.createNewView({browser, prevBrowsers, comparedBrowsers});
  }.bind(this);

  this.$onInit = function() {
    // AngularJS does not seem to have reliable post-render callbacks.
    // Use $timeout as a hack.
    $timeout(function() {
      org.chromium.apis.web.TimeSeriesChart.create({
        $scope,
        title: this.title,
        chartId: this.chartId,
        metric: this.failureToShipMetric,
        valueName: 'numFailureToShip',
        tooltipGen: function(d) {
          return {
            browserName: d.browser.browserName,
            properties: [
              {
                name: 'version',
                value: d.browserVersion,
              },
              {
                name: 'falure to ship',
                value: d.numFailureToShip,
                action: function() {
                  createNewView(d.browser, d.prevReleaseBrowsers,
                    d.comparedBrowsers);
                },
              },
            ],
          };
        },
      });
    }.bind(this));
  };
}

angular.module('confluence').component('failureToShipChart', {
  template: require('../../static/component/chart.html'),
  controller: ['$scope', '$timeout', failureToShipChartController],
  bindings: {
    failureToShipMetric: '<',
    title: '@',
    chartId: '@',
    createNewView: '&',
  },
});
