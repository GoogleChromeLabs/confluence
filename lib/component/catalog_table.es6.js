// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

require('angular');

require('../client/events.es6.js');
const pkg = org.chromium.apis.web;

function catalogTableController($scope, api) {
  // Do not indicate that data is loaded until both:
  // (1) DATA is set: apiMatrixData has arrived from worker;
  // AND
  // (2) NETWORK is set: Network request for fresh data has completed.
  //
  // TODO(markdittmer): This is too ad hoc. Clean "data loaded" signalling
  // logic.
  const WAIT_FLAGS = {
    DATA: 1 << 0,
    NETWORK: 1 << 1,
  };

  this.releaseOptions = null;
  this.numAvailable = null;

  this.waitState = 0;
  this.isLoaded = function() {
    return (this.waitState & WAIT_FLAGS.DATA) &&
        (this.waitState & WAIT_FLAGS.NETWORK);
  };

  $scope.currentPage = 0;
  $scope.gap = 5;
  $scope.itemPerPage = 15;
  $scope.searchKey = '';
  $scope.apiCatalogMatrix = {};
  $scope.showRows = {};

  const controller = this;
  const matrixController = api.apiMatrixController;
  const workerEvents = api.workerEvents;

  // An helper function to get releaseKeys from an
  // array of release objects.
  const getReleaseKeys = function(releases) {
    return releases.map((release) => {
      return release.releaseKey;
    });
  };

  const setReleases = function(releases) {
    const releaseKeys = getReleaseKeys(releases);
    matrixController.setReleaseKeys(releaseKeys);
    controller.releaseKeys = releaseKeys;

    // Wait for all updates to arrive from worker.
    controller.waitState = 0;
  };
  const setOpts = function(opts) {
    matrixController.setOpts(opts);

    // Wait exclusively for "DATA" update to arrive from worker.
    controller.waitState = controller.waitState & (~WAIT_FLAGS.DATA);
  };
  const setReleasesAndOpts = function(releases, opts) {
    setReleases(releases);
    const releasesWaitState = controller.waitState;
    setOpts(opts);
    const optsWaitState = controller.waitState;

    // Wait for most conservative set of updates determined by individual calls.
    controller.waitState = releasesWaitState & optsWaitState;
  };

  const setPageLength = function() {
    $scope.gap = 5;
    $scope.itemPerPage = 15;
    $scope.pageLength = Math.ceil(Object.keys($scope.apiCatalogMatrix).length /
                                  $scope.itemPerPage);
    $scope.currentPage = Math.max(
        0, Math.min($scope.currentPage, $scope.pageLength - 1));
  };

  const displayMatrix = function(matrix) {
    $scope.apiCatalogMatrix = matrix;
    setPageLength();
    $scope.$apply();
    api.statsController.onDisplayMatrix(matrix);
  };

  workerEvents.sub(function(_, event) {
    // Only display a matrix that is consistent with current configuration.
    if (pkg.NewMatrixEvent.isInstance(event) &&
        foam.util.equals(event.releaseKeys, controller.releaseKeys) &&
        foam.util.equals(event.searchKey, $scope.searchKey) &&
        foam.util.equals(event.releaseOptions, controller.releaseOptions) &&
        foam.util.equals(event.numAvailable, controller.numAvailable)) {
      controller.waitState |= WAIT_FLAGS.DATA;
      displayMatrix(event.matrix);
    }

    if (pkg.DataLoadedEvent.isInstance(event)) {
      controller.waitState |= WAIT_FLAGS.NETWORK;
    }

    $scope.$apply();
  });

  // Use indicator to debounce transient changes during initialization.
  let initialized = false;
  controller.$onInit = function() {
    initialized = true;

    api.bindState(this, 'releaseOptions', null);
    api.bindState(this, 'numAvailable', null);
    api.bindState($scope, 'currentPage', 0);
    api.bindState($scope, 'gap', 5);
    api.bindState($scope, 'itemPerPage', 15);
    api.bindState($scope, 'searchKey', '');

    setReleasesAndOpts(controller.releases, {
      searchKey: $scope.searchKey,
      releaseOptions: controller.releaseOptions,
      numAvailable: controller.numAvailable,
    });
  };

  controller.$onChanges = function(changes) {
    // Use this handler only after $onInit.
    if (!initialized) return;

    if (changes.releases &&
        !foam.util.equals(changes.releases.previousValue,
                          changes.releases.currentValue)) {
      setReleases(changes.releases.currentValue);
    }
    if (changes.releaseOptions || changes.numAvailable) {
      // Prevent unbound values from coercing null to undefined; undefined
      // causes problems for foam.util.equals().
      controller.releaseOptions = controller.releaseOptions || null;
      controller.numAvailable = controller.numAvailable || null;

      setOpts({
        searchKey: $scope.searchKey,
        releaseOptions: controller.releaseOptions,
        numAvailable: controller.numAvailable,
      });
    }
  };

  // Get interface names from itemPerPage * current page
  // to itemPerPage * (currentPage + 1)
  $scope.getInterfaceRange = function() {
    return Object.keys(this.apiCatalogMatrix).sort().slice(
      $scope.currentPage * $scope.itemPerPage,
      ($scope.currentPage + 1) * $scope.itemPerPage);
  };
  $scope.showCatalog = function(interfaceName) {
    $scope.showRows[interfaceName] = ! $scope.showRows[interfaceName];
  };
  // When remove release button clicked, notify
  // parent component to delete this release from release list.
  $scope.removeRelease = function($event, release) {
    $event.stopPropagation();
    controller.onDeleteRelease({release});
  };
  // Get page range from currentPage - 2 to currentPage + 2.
  $scope.range = function(size, current) {
    let ret = [];
    let start = current - 2;
    if (start < 0) start = 0;
    let end = start + $scope.gap;
    if (end > size) {
      end = size;
      start = size - $scope.gap;
      if (start < 0) start = 0;
    }
    for (let i = start; i < end; i++) {
      ret.push(i);
    }
    return ret;
  };
  // Set page when navigate to a new page.
  $scope.setPage = function(p) {
    if ($scope.currentPage === 'undefined') return;
    if (p < 0 || p >= $scope.pageLength) return;
    $scope.currentPage = p;
  };
  // Download csv from catalogMatrix. This matrix
  // is filtered by release options and search keyword (if exists).
  $scope.downloadCSV = function() {
    let filename = 'result.csv';
    let releaseKeys = getReleaseKeys(controller.releases);
    matrixController.getMatrixCSV(releaseKeys).then(csv => {
      if (csv === null) return;
      if (!csv.match(/^data:text\/csv/i)) {
        csv = 'data:text/csv;charset=utf-8,' + csv;
      }
      let data = encodeURI(csv);
      let link = document.createElement('a');
      link.style.display = 'none';
      link.setAttribute('href', data);
      link.setAttribute('download', filename);
      document.body.appendChild(link);
      link.click();
    });
  };
  // Get searched result from apiMatrix and update view.
  $scope.search = function($event) {
    $event.preventDefault();
    setOpts({
      searchKey: $scope.searchKey,
      releaseOptions: controller.releaseOptions,
      numAvailable: controller.numAvailable,
    });
  };
}

angular.module('confluence').component('apiCatalogTable', {
  template: require('../../static/component/catalog_table.html'),
  controller: ['$scope', 'api', catalogTableController],
  bindings: {
    // release is an array of releases.
    releases: '<',
    // releaseOptions is an optional JSON of form {releaseKey: Boolean, ...}
    // The result matrix will be filtered based on the options.
    releaseOptions: '<',
    // Function to handle delete release action if ableToDeleteRelease is true.
    onDeleteRelease: '&',
    // ableToDeleteRelease is a Boolean indicate if the releases
    // in this table are able to be deleted.
    ableToDeleteRelease: '@',
    // numAvailable is an optional Integer or Integer array. When set, only
    // APIs supported by numAvailable releases are returned. When numAvailable
    // is an array, any integer in it is a valid number of supporting releases.
    numAvailable: '<',
  },
});
