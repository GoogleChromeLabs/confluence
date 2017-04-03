// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

function catalogTableController($scope, api) {
  $scope.apiCatalogMatrix = {};
  $scope.showRows = {};
  $scope.currentPage = 0;
  $scope.gap = 5;
  $scope.itemPerPage = 15;
  let ctrl = this;
  let apiMatrix = api.matrix;
  let setPageLength = function() {
    $scope.currentPage = 0;
    $scope.gap = 5;
    $scope.itemPerPage = 15;
    $scope.pageLength = Math.ceil(Object.keys($scope.apiCatalogMatrix).length
      / $scope.itemPerPage);
  };
  let displayMatrix = function(matrix) {
    $scope.apiCatalogMatrix = matrix;
    setPageLength();
    $scope.$apply();
  };
  let getBrowserKeys = function(browsers) {
    return browsers.map((browser) => {
      return browser.browserKey;
    });
  };

  ctrl.$onChanges = function(changes) {
    if (changes.browsers && changes.browsers.currentValue) {
      console.log(changes.browsers)
      let browserKeys = getBrowserKeys(changes.browsers.currentValue);
      apiMatrix.toMatrix(browserKeys, {
        browserOptions: ctrl.browserOptions,
        numAvailable: ctrl.numAvailable,
      }).then(displayMatrix);
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
  $scope.removeBrowser = function($event, browser) {
    $event.stopPropagation();
    console.log(browser);
    ctrl.onDeleteBrowser({browser});
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
    let browserKeys = getBrowserKeys(ctrl.browsers);
    let csv = apiMatrix.matrixToCSV(browserKeys,
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
    let browserKeys = getBrowserKeys(ctrl.browsers);
    let key = $scope.searchKey;
    apiMatrix.toMatrix(browserKeys, {
      searchKey: key,
      browserOptions: ctrl.browserOptions,
      numAvailable: ctrl.numAvailable,
    }).then(displayMatrix);
  };
}

angular.module('confluence').component('apiCatalogTable', {
  template: require('../../static/component/catalog_table.html'),
  controller: ['$scope', 'api', catalogTableController],
  bindings: {
    // browser is an array of browsers.
    browsers: '<',
    // browserOptions is an optional JSON of form {browserKey: Boolean, ...}
    // The result matrix will be filtered based on the options.
    browserOptions: '<',
    // Function to handle delete browser action if ableToDeleteBrowser is true.
    onDeleteBrowser: '&',
    // ableToDeleteBrowser is a Boolean indicate if the browsers
    // in this table are able to be deleted.
    ableToDeleteBrowser: '@',
    // numAvailable is an optional Integer or Integer array property that the
    // result interface/API must be available in this numAvailable number(s)
    // of browsers.
    numAvailable: '<',
  },
});
