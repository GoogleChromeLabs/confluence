// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

function catalogTableController($scope) {
  $scope.apiCatalogMatrix = {};
  $scope.showRows = {};
  $scope.currentPage = 0;
  $scope.gap = 5;
  $scope.itemPerPage = 15;
  let ctrl = this;
  let setPageLength = function() {
    $scope.currentPage = 0;
    $scope.gap = 5;
    $scope.itemPerPage = 15;
    $scope.pageLength = Math.ceil(Object.keys($scope.apiCatalogMatrix).length
      / $scope.itemPerPage);
  };
  let disPlayMatrix = function(matrix) {
    $scope.apiCatalogMatrix = matrix;
    setPageLength();
    $scope.$apply();
  };
  ctrl.$onChanges = function(changes) {
    if (changes.browserKeys && changes.browserKeys.currentValue != null) {
      ctrl.apiMatrix.toMatrix(changes.browserKeys.currentValue, {
        browserOptions: ctrl.browserOptions,
        length: ctrl.length,
        lengths: ctrl.lengths,
      })
      .then(disPlayMatrix);
    }
  };
  $scope.getInterfaceRange = function() {
    return Object.keys(this.apiCatalogMatrix).sort().slice(
      $scope.currentPage * $scope.itemPerPage,
      ($scope.currentPage + 1) * $scope.itemPerPage);
  };
  $scope.showCatalog = function(interfaceName) {
    if ($scope.showRows[interfaceName]) {
      $scope.showRows[interfaceName] = false;
    } else {
      $scope.showRows[interfaceName] = true;
    }
  };
  $scope.removeBrowser = function($event, browserKey) {
    $event.stopPropagation();
    ctrl.onDeleteBrowserKey({browserKey});
  };
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
  $scope.setPage = function(p) {
    if ($scope.currentPage === undefined) return;
    if (p < 0 || p >= $scope.pageLength) return;
    $scope.currentPage = p;
  };
  $scope.downloadCSV = function() {
    let filename = 'result.csv';
    let csv = ctrl.apiMatrix.matrixToCSV(ctrl.browserKeys,
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
  $scope.search = function($event) {
    $event.preventDefault();
    let key = $scope.searchKey;
    ctrl.apiMatrix.toMatrix(ctrl.browserKeys, {
      searchKey: key,
      browserOptions: ctrl.browserOptions,
      length: ctrl.length,
      lengths: ctrl.lengths,
    }).then(disPlayMatrix);
  };
}

angular.module('confluence').component('apiCatalogTable', {
  template: require('../../static/component/catalog_table.html'),
  controller: catalogTableController,
  bindings: {
    // apiMatrix is an instance of org.chromium.apis.web.
    apiMatrix: '<',
    // browserKeys is an String array of valid browser keys.
    browserKeys: '<',
    // browserOptions is an optional JSON of form {browserKey: Boolean, ...}
    // The result matrix will be filtered based on the options.
    browserOptions: '<',
    // onDeleteBrowserKey is a Boolean indicate if the browsers
    // in this table are able to be deleted.
    onDeleteBrowserKey: '&',
    // Function to handle delete browser action if onDeleteBrowserKey is true.
    ableToDeleteBrowser: '@',
    // numAvailable is an optional Integer or Integer array property that the
    // result interface/API must be available in this numAvailable number(s)
    // of browsers.
    numAvailable: '<',
  },
});
