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

  require('../../lib/chart/api_count_chart.es6.js');
  require('../../lib/chart/browser_metric_chart.es6.js');
  require('../../lib/chart/time_series_chart.es6.js');
  require('../../lib/client/api_confluence.es6.js');
  require('../../lib/client/api_service.es6.js');
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
  require('../../lib/object.es6.js');
  require('../../lib/property.es6.js');
  require('../../lib/action.es6.js');

  if (foam.isServer) requireNodeCode();
  else requireBrowserCode();

  require('../../lib/client/stats_controller.es6.js');
  require('../../lib/compat.es6.js');
  require('../../lib/confluence/lone_removal.es6.js');
  require('../../lib/confluence/api_count.es6.js');
  require('../../lib/confluence/api_count_data.es6.js');
  require('../../lib/confluence/browser_metric_data.es6.js');
  require('../../lib/confluence/browser_specific.es6.js');
  require('../../lib/confluence/lone_omission.es6.js');
  require('../../lib/confluence/metric_computer.es6.js');
  require('../../lib/confluence/metric_computer_runner.es6.js');
  require('../../lib/confluence/metric_computer_service.es6.js');
  require('../../lib/confluence/set_ops.es6.js');
  require('../../lib/dao/api_service_dao.es6.js');
  require('../../lib/dao/dao_container.es6.js');
  require('../../lib/dao/grid_dao.es6.js');
  require('../../lib/dao/http_json_dao.es6.js');
  require('../../lib/dao/indexed_dao.es6.js');
  require('../../lib/dao/json_dao_container.es6.js');
  require('../../lib/dao/worker_dao.es6.js');
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
  const pkg = org.chromium.apis.web;

  foam.CLASS(require('../data/class:org.chromium.apis.web.generated.CompatData.json'));
});
