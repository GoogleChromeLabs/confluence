// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

require('angular');

require('../compat.es6.js');
require('../component/catalog_table.es6.js');
const pkg = org.chromium.apis.web;

angular.module('confluence').controller('catalogController', ['api',
  function(api) {
    const compatClassURL = `${window.location.origin}/${pkg.DAOContainer.COMPAT_MODEL_FILE_NAME}`;
    pkg.ClassGenerator.create({
      classURL: compatClassURL,
    }).generateClass().then(CompatData => {
      const compatDAO = api.getApiServiceDAO(
          pkg.DAOContainer.COMPAT_NAME, CompatData);
      
      // TODO(markdittmer): Setup and render DAOController.
      compatDAO.skip(0).limit(200).select().then(sink => {
        console.log('Got initial data', sink, performance.now());
      })
    });
  }]);
