// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

require('angular');

require('../chart/time_series_chart.es6.js');
require('../chart/api_velocity_chart.es6.js');
require('../chart/browser_metric_chart.es6.js');
require('../client/state.es6.js');
require('../parse/parser_interpreter.es6.js');
const pkg = org.chromium.apis.web;
const parseUtil = org.chromium.parse.util;

angular.module('confluence')
  .controller('confluenceController', ['$scope', 'api', function($scope, api) {
    api.urlState.clearBindings();

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
        query += `${opts[id] ? 'in:' : 'notin:'}${parseUtil.getTerseReleaseId(id)} `;
      }
      const counts = state.numAvailable;
      if (counts && counts.length !== 0) {
        if (counts.length === 1) {
          query += `count:${counts[0]}`;
        } else {
          query += '(';
          query += counts.map(count => `count:${count}`).join(' or ');
          query += ')';
        }
      }
      if (counts && counts.length === 1) {
        query += `count:${counts[0]}`;
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
