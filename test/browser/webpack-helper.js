/**
 * @license
 * Copyright 2017 Google Inc. All Rights Reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the 'License');
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an 'AS IS' BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

/**
 * This helper program loads all files under libs using webpack.
 * Then exports objects needed in test as global objects.
 * Thus, libraries like ObjectGraph will not be executed twice in karma.
 */

// Import libraries to test.
require('../../lib/web_catalog/api_extractor.es6');
require('../../lib/web_apis/browser.es6');
require('../../lib/web_apis/web_interface.es6');

foam.RELATIONSHIP({
  sourceModel: 'org.chromium.apis.web.Browser',
  targetModel: 'org.chromium.apis.web.WebInterface',
  forwardName: 'interface',
  inverseName: 'browser',
  cardinality: '*:*',
});

require('../../lib/web_apis/api_importer.es6');
require('../../lib/client/api_matrix.es6');

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
