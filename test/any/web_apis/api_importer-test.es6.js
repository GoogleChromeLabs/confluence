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
'use strict';

describe('ApiImporter', function() {
  let apiImporter = org.chromium.apis.web.ApiImporter.create();
  let mlang = foam.mlang.ExpressionsSingleton.create();
  let webCatalog = {
    'Window': [
      'Function',
      'property',
    ],
    'Function': [
      'arguments',
      'caller',
    ],
  };

  apiImporter.import('Chrome', '56.0.2924.87',
    'OSX', '10.12.2', webCatalog);
  it('correctly imports browserAPIs to DAO', function(done) {
    let promises = [
      apiImporter.browserAPIs.find([
        'Chrome',
        '56.0.2924.87',
        'OSX',
        '10.12.2',
        'Window',
        'Function',
      ]),
      apiImporter.browserAPIs.find([
        'Chrome',
        '56.0.2924.87',
        'OSX',
        '10.12.2',
        'Window',
        'property',
      ]),
      apiImporter.browserAPIs.find([
        'Chrome',
        '56.0.2924.87',
        'OSX',
        '10.12.2',
        'Function',
        'arguments',
      ]),
      apiImporter.browserAPIs.find([
        'Chrome',
        '56.0.2924.87',
        'OSX',
        '10.12.2',
        'Function',
        'caller',
      ]),
    ];
    Promise.all(promises).then((results) => {
      results.forEach((browserAPI) => {
        expect(browserAPI).not.toBeNull();
      });
      return apiImporter.browserAPIs.select(mlang.COUNT());
    }).then((countSink) => {
      expect(countSink.value).toBe(promises.length);
      done();
    });
  });
});
