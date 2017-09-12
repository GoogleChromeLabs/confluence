// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

require('angular');

angular.module('confluence').controller(
    'headerController', [
      '$scope', '$transitions', '$state',
      function($scope, $transitions, $state) {
        const DEFAULT_LABEL = '';
        const HEADER_LABELS = {
          catalog: 'API Catalog',
          confluence: 'Confluence Metrics',
        };
        $scope.headerLabel = '';
        $transitions.onSuccess({}, function(transition) {
          transition.promise.then(function(data) {
            $scope.headerLabel = HEADER_LABELS[data.name] || DEFAULT_LABEL;
          });
        });
      }]);
