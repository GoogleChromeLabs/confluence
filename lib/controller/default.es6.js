// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

require('angular');

// Provide controller for default view that will depend on API service.
angular.module('confluence').controller('defaultController', [
  '$scope',
  'api',
  function($scope, api) {
    // No URL state on default page.
    api.clearState();
  }
]);
