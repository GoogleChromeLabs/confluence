// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

require('angular');

function vendorSpecificChartController($scope, $timeout) {
  let createNewView = function(browser, prevBrowsers, comparedBrowsers) {
    this.createNewView({browser, prevBrowsers, comparedBrowsers});
  }.bind(this);

  this.$postLink = function() {
    // AngularJS does not seem to have reliable post-render callbacks.
    // Use $timeout as a hack.
    $timeout(function() {
      org.chromium.apis.web.TimeSeriesChart.create({
        $scope,
        title: this.title,
        chartId: this.chartId,
        metric: this.vendorSpecificMetric,
        valueName: 'numVendorSpecific',
        tooltipGen: function(d) {
          return {
            browserName: d.browser.browserName,
            properties: [
              {
                name: 'version',
                value: d.browser.browserVersion,
              },
              {
                name: 'falure to ship',
                value: d.numVendorSpecific,
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

angular.module('confluence').component('vendorSpecificChart', {
  template: require('../../static/component/chart.html'),
  controller: ['$scope', '$timeout', vendorSpecificChartController],
  bindings: {
    vendorSpecificMetric: '<',
    title: '@',
    chartId: '@',
    createNewView: '&',
  },
});
