// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

require('angular');

function browserSpecificChartController($scope, $timeout) {
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
        metric: this.browserSpecificMetric,
        valueName: 'numBrowserSpecific',
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
                value: d.numBrowserSpecific,
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

angular.module('confluence').component('browserSpecificChart', {
  template: require('../../static/component/chart.html'),
  controller: ['$scope', '$timeout', browserSpecificChartController],
  bindings: {
    browserSpecificMetric: '<',
    title: '@',
    chartId: '@',
    createNewView: '&',
  },
});
