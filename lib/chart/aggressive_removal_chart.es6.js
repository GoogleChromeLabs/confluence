// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

require('angular');

function aggressiveRemovalController($scope, $timeout) {
  let createNewView = function(releaseOneYearAgo, prevReleases,
      currReleases) {
    this.createNewView({releaseOneYearAgo, prevReleases, currReleases});
  }.bind(this);

  this.$postLink = function() {
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
            browserName: d.release.browserName,
            properties: [
              {
                name: 'version 1 year ago',
                value: d.release.browserVersion,
              },
              {
                name: 'Aggressive Removal',
                value: d.value,
                action: function() {
                  createNewView(d.release,
                    d.prevReleases,
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
