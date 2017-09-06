// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

require('angular');

require('../client/events.es6.js');
const pkg = org.chromium.apis.web;

function catalogTableController($scope, api) {
  $scope.apiCatalogMatrix = {};
  $scope.showRows = {};
  const controller = this;
  const matrixController = api.apiMatrixController;
  const workerEvents = api.workerEvents;

  this.$onInit = function() {
    // HACK(markdittmer): Do not bind to global URL state unless this is the
    // API catalog page.
    if (api.getPath() !== '!/catalog') return;

    api.bindState(this, 'releaseOptions');
    api.bindState(this, 'numAvailable');
    api.bindState($scope, 'currentPage', 0);
    api.bindState($scope, 'gap', 5);
    api.bindState($scope, 'itemPerPage', 15);
    api.bindState($scope, 'searchKey', '');
    matrixController.setOpts({
      searchKey: $scope.searchKey,
      releaseOptions: controller.releaseOptions,
      numAvailable: controller.numAvailable,
    });
  };

  const setPageLength = function() {
    $scope.gap = 5;
    $scope.itemPerPage = 15;
    $scope.pageLength = Math.ceil(Object.keys($scope.apiCatalogMatrix).length /
                                  $scope.itemPerPage);
    $scope.currentPage = Math.min($scope.currentPage, $scope.pageLength - 1);
  };

  // An helper function to get releaseKeys from an
  // array of release objects.
  let getReleaseKeys = function(releases) {
    return releases.map((release) => {
      return release.releaseKey;
    });
  };

  const setReleases = function(releases) {
    const releaseKeys = getReleaseKeys(releases);
    matrixController.setReleaseKeys(releaseKeys);
    controller.releaseKeys = releaseKeys;
  };

  const displayMatrix = function(matrix) {
    $scope.apiCatalogMatrix = matrix;
    setPageLength();
    $scope.$apply();
  };

  workerEvents.sub(function(_, event) {
    if (pkg.NewMatrixEvent.isInstance(event)) displayMatrix(event.matrix);
  });

  controller.$onChanges = function(changes) {
    if (changes.releases &&
        !foam.util.equals(changes.releases.previousValue,
                          changes.releases.currentValue)) {
      setReleases(changes.releases.currentValue);
    }
    if (changes.releaseOptions || changes.numAvailable) {
      matrixController.setOpts({
        searchKey: $scope.searchKey,
        releaseOptions: changes.releaseOptions ?
            changes.releaseOptions.currentValue : controller.releaseOptions,
        numAvailable: changes.numAvailable ?
            changes.numAvailable.currentValue : controller.numAvailable,
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
      link.setAttribute('href', data);
      link.setAttribute('download', filename);
      link.click();
    });
  };
  // Get searched result from apiMatrix and update view.
  $scope.search = function($event) {
    $event.preventDefault();
    matrixController.setOpts({
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
    // Function to handle delete release action if ableToDeleteRelease is true.
    onDeleteRelease: '&',
    // ableToDeleteRelease is a Boolean indicate if the releases
    // in this table are able to be deleted.
    ableToDeleteRelease: '@',
  },
});
