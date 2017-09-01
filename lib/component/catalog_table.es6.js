// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

require('angular');

require('../dao_container.es6.js');
const pkg = org.chromium.apis.web;

function catalogTableController($scope, api) {
  $scope.apiCatalogMatrix = {};
  $scope.showRows = {};
  $scope.currentPage = 0;
  $scope.gap = 5;
  $scope.itemPerPage = 15;
  const ctrl = this;
  const apiMatrix = api.matrix;
  const workerEvents = api.workerEvents;
  let setPageLength = function() {
    $scope.currentPage = 0;
    $scope.gap = 5;
    $scope.itemPerPage = 15;
    $scope.pageLength = Math.ceil(Object.keys($scope.apiCatalogMatrix).length /
                                  $scope.itemPerPage);
  };

  let displayMatrix = function(matrix) {
    $scope.apiCatalogMatrix = matrix;
    setPageLength();
    $scope.$apply();
  };

  let resetMatrix = function(releaseKeys, opts) {
    // Ensure that base API data are loaded before attempting to construct a
    // matrix.
    api.apiDAO
        .then(() => apiMatrix.toMatrix(releaseKeys, opts).then(displayMatrix));
  };

  // An helper function to get releaseKeys from an
  // array of release objects.
  let getReleaseKeys = function(releases) {
    return releases.map((release) => {
      return release.releaseKey;
    });
  };

  // Update matrix and view when releases was changed.
  let onReleaseChanges = function(releaseKeys) {
    resetMatrix(releaseKeys, {
      releaseOptions: ctrl.releaseOptions,
      numAvailable: ctrl.numAvailable,
    });
  };
  ctrl.$onChanges = function(changes) {
    // Do not perform expensive onReleaseChanges() unless changing from a
    // non-empty collection of releases.
    if (changes.releases && changes.releases.previousValue &&
        changes.releases.previousValue.length &&
        changes.releases.currentValue) {
      onReleaseChanges(getReleaseKeys(changes.releases.currentValue));
    }
  };

  // Update matrix and view when new data for current releases is loaded in
  // worker.
  api.latestAPIData.then(() => onReleaseChanges(getReleaseKeys(ctrl.releases)));

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
    ctrl.onDeleteRelease({release});
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
    let releaseKeys = getReleaseKeys(ctrl.releases);
    let csv = apiMatrix.matrixToCSV(releaseKeys,
      $scope.apiCatalogMatrix);
    if (csv === null) return;
    if (!csv.match(/^data:text\/csv/i)) {
      csv = 'data:text/csv;charset=utf-8,' + csv;
    }
    let data = encodeURI(csv);
    let link = document.createElement('a');
    link.setAttribute('href', data);
    link.setAttribute('download', filename);
    link.click();
  };
  // Get searched result from apiMatrix and update view.
  $scope.search = function($event) {
    $event.preventDefault();
    let releaseKeys = getReleaseKeys(ctrl.releases);
    let key = $scope.searchKey;
    resetMatrix(releaseKeys, {
      searchKey: key,
      releaseOptions: ctrl.releaseOptions,
      numAvailable: ctrl.numAvailable,
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
