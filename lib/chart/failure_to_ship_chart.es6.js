// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

require('angular');

function failureToShipChartController($scope, $timeout) {
  let createNewView = function(release, prevReleases, comparedReleases) {
    this.createNewView({release, prevReleases, comparedReleases});
  }.bind(this);

  this.$postLink = function() {
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
            browserName: d.release.browserName,
            properties: [
              {
                name: 'version',
                value: d.release.browserVersion,
              },
              {
                name: 'falure to ship',
                value: d.numFailureToShip,
                action: function() {
                  createNewView(d.release, d.prevReleases,
                      d.comparedReleases);
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
