// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

// Manually require code under test (in alphabetical order).
//
// TODO(markdittmer): Move to FOAM classloader for this.

beforeAll(function() {
  // Use of foam.isServer = <running in Node JS?>:
  // - Do not load Angular-related in Node JS (foam.isServer = true).
  // - Do not load import-from-filesystem in browser.

  if (!foam.isServer) {
    // TODO(markittmer): Fix: Alphabetical order exception: main/app defines
    // angular module used by other code.
    require('../../main/app.es6.js');

    require('../../lib/chart/aggressive_removal_chart.es6.js');
    require('../../lib/chart/api_velocity_chart.es6.js');
    require('../../lib/chart/failure_to_ship_chart.es6.js');
    require('../../lib/chart/time_series_chart.es6.js');
    require('../../lib/chart/vendor_specific_chart.es6.js');
    require('../../lib/client/api_confluence.es6.js');
  }

  // Client-side, but used in server integration tests.
  require('../../lib/client/api_matrix.es6.js');

  if (!foam.isServer) {
    require('../../lib/client/api_service.es6.js');
    require('../../lib/component/api_analytics.es6.js');
    require('../../lib/component/catalog_table.es6.js');
  }

  require('../../lib/confluence/aggressive_removal_data.es6.js');
  require('../../lib/confluence/aggressive_removal.es6.js');
  require('../../lib/confluence/api_velocity_data.es6.js');
  require('../../lib/confluence/api_velocity.es6.js');
  require('../../lib/confluence/failure_to_ship_data.es6.js');
  require('../../lib/confluence/failure_to_ship.es6.js');
  require('../../lib/confluence/metric_computer.es6.js');
  require('../../lib/confluence/vendor_specific_data.es6.js');
  require('../../lib/confluence/vendor_specific.es6.js');

  if (!foam.isServer) {
    require('../../lib/controller/api_catalog.es6.js');
    require('../../lib/controller/api_confluence.es6.js');
  }

  if (foam.isServer) {
    require('../../lib/datastore/datastore_importer.es6.js');
  }

  require('../../lib/datastore/rate_limited_DAO.es6.js');
  require('../../lib/datastore/rate_limiter.es6.js');
  require('../../lib/web_apis/api_importer.es6.js');
  require('../../lib/web_apis/browser.es6.js');
  require('../../lib/web_apis/browser_interface_relationship.es6.js');
  require('../../lib/web_apis/version_history.es6.js');
  require('../../lib/web_apis/web_interface.es6.js');
  require('../../lib/web_catalog/api_extractor.es6.js');
});
