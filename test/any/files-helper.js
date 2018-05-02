// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

// Manually require code under test (in alphabetical order).
//
// TODO(markdittmer): Move to FOAM classloader for this.

function requireBrowserCode() {
  // Angular-related (browser-only) code.
  //
  // TODO(markittmer): Fix: Alphabetical order exception: main/app defines
  // angular module used by other browser-only code below.
  require('../../main/app.es6.js');

  require('../../lib/chart/api_velocity_chart.es6.js');
  require('../../lib/chart/browser_metric_chart.es6.js');
  require('../../lib/chart/time_series_chart.es6.js');
  require('../../lib/client/api_confluence.es6.js');
  require('../../lib/client/api_service.es6.js');
  require('../../lib/client/release_interface_dao.es6.js');
  require('../../lib/component/catalog_table.es6.js');
  require('../../lib/controller/api_catalog.es6.js');
  require('../../lib/controller/api_confluence.es6.js');
  require('../../lib/controller/default.es6.js');
}

function requireNodeCode() {
  // Code invoking Node JS APIs.
  require('../../lib/dao/local_json_dao.es6.js');
  require('../../lib/server/server.es6.js');
  require('../../lib/web_catalog/api_extractor_service.es6.js');
  require('../../lib/web_catalog/object_graph_importer.es6.js');
}

beforeAll(function() {
  // Load refinements before anything else.
  require('../../lib/property.es6.js');

  if (foam.isServer) requireNodeCode();
  else requireBrowserCode();

  // Client-side, but used in server integration tests.
  require('../../lib/client/api_matrix.es6.js');

  require('../../lib/client/stats_controller.es6.js');
  require('../../lib/compat.es6.js');
  require('../../lib/confluence/aggressive_removal.es6.js');
  require('../../lib/confluence/api_velocity.es6.js');
  require('../../lib/confluence/api_velocity_data.es6.js');
  require('../../lib/confluence/browser_metric_data.es6.js');
  require('../../lib/confluence/browser_specific.es6.js');
  require('../../lib/confluence/failure_to_ship.es6.js');
  require('../../lib/confluence/metric_computer.es6.js');
  require('../../lib/confluence/metric_computer_runner.es6.js');
  require('../../lib/confluence/metric_computer_service.es6.js');
  require('../../lib/confluence/set_ops.es6.js');
  require('../../lib/dao/dao_container.es6.js');
  require('../../lib/dao/grid_dao.es6.js');
  require('../../lib/dao/http_json_dao.es6.js');
  require('../../lib/dao/indexed_dao.es6.js');
  require('../../lib/dao/json_dao_container.es6.js');
  require('../../lib/parse/expressions.es6.js');
  require('../../lib/parse/parser_interpreter.es6.js');
  require('../../lib/web_apis/api_compat_data.es6.js');
  require('../../lib/web_apis/api_importer.es6.js');
  require('../../lib/web_apis/relational_to_compat.es6.js');
  require('../../lib/web_apis/release.es6.js');
  require('../../lib/web_apis/release_interface_relationship.es6.js');
  require('../../lib/web_apis/version_history.es6.js');
  require('../../lib/web_apis/web_interface.es6.js');
  require('../../lib/web_catalog/api_extractor.es6.js');
});
