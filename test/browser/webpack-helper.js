// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

/**
 * This helper program loads all files under libs using webpack.
 * Then exports objects needed in test as global objects.
 * Thus, libraries like ObjectGraph will not be executed twice in karma.
 */

// Import libraries to test.
require('foam2/test/helpers/testcontext');
require('../../lib/web_catalog/api_extractor.es6.js');
require('../../lib/web_apis/browser.es6.js');
require('../../lib/web_apis/web_interface.es6.js');
require('../../lib/web_apis/browser_interface_relationship.es6.js');
require('../../lib/web_apis/api_importer.es6.js');
require('../../lib/client/api_matrix.es6.js');
require('../../lib/datastore/rate_limiter.es6.js');
require('../../lib/datastore/rate_limited_DAO.es6.js');

// Require external libraries.
let libs = {
  ObjectGraph: require('object-graph-js').ObjectGraph,
  DATA: {
	chrome56: require('../data/window_Chrome_56_Windows_10.0.json'),
	edge14: require('../data/window_Edge_14_Windows_10.0.json'),
	safari602: require('../data/window_Safari_602_OSX_10.12.3.json'),
  },
};

// Add external libraries to global object.
if (typeof global === 'undefined') global = window;

Object.keys(libs).forEach((key) => {
  global[key] = libs[key];
});
