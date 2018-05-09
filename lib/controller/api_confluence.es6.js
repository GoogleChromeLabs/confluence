// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

require('angular');

require('../chart/time_series_chart.es6.js');
require('../chart/api_velocity_chart.es6.js');
require('../chart/browser_metric_chart.es6.js');
require('../client/state.es6.js');
const pkg = org.chromium.apis.web;

angular.module('confluence')
  .controller('confluenceController', ['$scope', 'api', function($scope, api) {
    // Activate tabs.
    $('ul.tabs').tabs();
    $scope.showTab = 0;
    $scope.apiVelocityMetrics = {};
    $scope.browserMetric = null;
    $scope.failureToShipMetric = null;
    $scope.browserSpecificMetric = null;
    $scope.aggressiveRemovalMetric = null;
    $scope.additionalViews = [];
    const apiConfluence = pkg.ApiConfluence.create({
      apiVelocityDAO: api.getApiServiceDAO(
          pkg.DAOContainer.API_VELOCITY_NAME, null, null),
      failureToShipDAO: api.getApiServiceDAO(
          pkg.DAOContainer.FAILURE_TO_SHIP_NAME, null, null),
          browserSpecificDAO: api.getApiServiceDAO(
          pkg.DAOContainer.BROWSER_SPECIFIC_NAME, null, null),
      aggressiveRemovalDAO: api.getApiServiceDAO(
          pkg.DAOContainer.AGGRESSIVE_REMOVAL_NAME, null, null),
    });

    apiConfluence.getApiVelocity().then((apiVelocityMetrics) => {
      $scope.apiVelocityMetrics = apiVelocityMetrics;
      $scope.$apply();
    });

    apiConfluence.getFailureToShip().then((failureToShipMetric) => {
      $scope.failureToShipMetric = failureToShipMetric;
      $scope.$apply();
    });

    apiConfluence.getBrowserSpecificApis().then((browserSpecificMetric) => {
      $scope.browserSpecificMetric = browserSpecificMetric;
      $scope.$apply();
    });

    apiConfluence.getAggressiveRemoval()
      .then((aggressiveRemovalMetric) => {
        $scope.aggressiveRemovalMetric = aggressiveRemovalMetric;
      });

    const getTerseReleaseId = function(id) {
      const parts = id.split('_').map(part => part.toLowerCase());
      foam.assert(parts.length === 4, `Unexpected release ID format: ${id}`);

      const browserVersion = parts[1];
      const browserVersionLastDot = browserVersion.lastIndexOf('.');
      foam.assert(browserVersionLastDot !== -1,
                  `Unexpected browser version ${browserVersion}`);
      const osVersion = parts[3];
      const osVersionLastDot = osVersion.lastIndexOf('.');
      foam.assert(osVersionLastDot !== -1,
                  `Unexpected os version ${osVersion}`);
      
                  parts[0] = parts[0].substr(0, 3);
      parts[1] = browserVersion.substr(0, browserVersionLastDot);
      parts[2] = parts[2].substr(0, 3);
      parts[3] = osVersion.substr(0, osVersionLastDot);
      
      return parts.join('');
    }
    const goToCatalog = function(state) {
      let urlState = foam.web.DetachedURLState.create();
      urlState.setPath('!/catalog');
      
      let catalogState = pkg.CatalogState.create({
        urlState,
        selectedReleases: state.releaseKeys || [],
      });
      const opts = state.releaseOptions;
      let query = '';
      for (const id of Object.keys(state.releaseOptions)) {
        query += `${opts[id] ? 'in:' : 'notin:'}${getTerseReleaseId(id)} `;
      }
      catalogState.query = query;
      
      const hash = urlState.getHash();
      let link = document.createElement('a');
      link.style.display = 'none';
      link.setAttribute('href', `${window.location.origin}/${hash}`);
      link.setAttribute('target', '_blank');
      document.body.appendChild(link);
      link.click();
    };

    $scope.createNewDiffView = function(minuend, subtrahend) {
      let releaseOptions = {};
      releaseOptions[minuend.releaseKey] = true;
      releaseOptions[subtrahend.releaseKey] = false;
      goToCatalog({
        releaseKeys: [minuend.releaseKey, subtrahend.releaseKey],
        releaseOptions,
      });
    };
    $scope.newFailureToShipView = function(release, prevs, others) {
      let releaseOptions = {};
      let releases = [release].concat(prevs, others);
      releaseOptions[release.releaseKey] = false;
      for (let i = 0; i < prevs.length; i++) {
        releaseOptions[prevs[i].releaseKey] = false;
      }
      for (let i = 0; i < others.length; i++) {
        releaseOptions[others[i].releaseKey] = true;
      }
      goToCatalog({
        releaseKeys: releases.map(release => release.releaseKey),
        releaseOptions,
      });
    };
    $scope.newBrowserSpecificView = function(release, prevs, others) {
      let releaseOptions = {};
      let releases = [release].concat(prevs, others);
      releaseOptions[release.releaseKey] = true;
      for (let i = 0; i < prevs.length; i++) {
        releaseOptions[prevs[i].releaseKey] = true;
      }
      for (let i = 0; i < others.length; i++) {
        releaseOptions[others[i].releaseKey] = false;
      }
      goToCatalog({
        releaseKeys: releases.map(release => release.releaseKey),
        releaseOptions,
      });
    };
    $scope.newAggressiveRemovalView = function(releaseOneYearAgo,
        prevReleases, currReleases) {
      let releases = [releaseOneYearAgo].concat(
          prevReleases, currReleases);
      let releaseOptions = {};
      // Set release options to be false for releaseOneYearAgo and
      // true for all currReleases. And set numAvailable to be 1 +
      // currReleases.length to releases.length. So it returns
      // APIs that not exists in releaseOneYearAgo but avaiable
      // in all current Releases and some prevReleases;
      releaseOptions[releaseOneYearAgo.releaseKey] = false;
      for (let i = 0; i < currReleases.length; i++) {
        releaseOptions[currReleases[i].releaseKey] = true;
      }
      let numAvailable = [];
      for (let i = currReleases.length + 1; i <= releases.length; i++) {
        numAvailable.push(i);
      }
      goToCatalog({
        releaseKeys: releases.map(release => release.releaseKey),
        releaseOptions,
        numAvailable,
      });
    };
  }]);
