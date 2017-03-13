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


/**
  @param {webpack-context} r - all files in the context will be loaded.
*/

// Import libraries to test.
require('../../lib/web_catalog/api_extractor.es6');
require('../../lib/web_apis_num_id/web_apis.es6');

// Require external libraries.
let libs = {
  ObjectGraph: require('object-graph-js').ObjectGraph,
};

// Add external libraries to global object.
if (typeof global === 'undefined') global = window;

Object.keys(libs).forEach((key) => {
  global[key] = libs[key];
});
