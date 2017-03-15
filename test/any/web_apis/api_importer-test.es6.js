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
  it('correctly imports browserAPIs to MDAO', function(done) {
    let interfaces = [
      {
        interfaceName: 'Window',
        apiName: 'Function',
      },
      {
        interfaceName: 'Window',
        apiName: 'property',
      },
      {
        interfaceName: 'Function',
        apiName: 'arguments',
      },
      {
        interfaceName: 'Function',
        apiName: 'caller',
      },
    ];
    let promises = interfaces.map((intface) => {
      return apiImporter.browserAPIs.where(mlang.AND(
          mlang.EQ(org.chromium.apis.web.BrowserAPI.BROWSER_NAME, 'Chrome'),
          mlang.EQ(org.chromium.apis.web.BrowserAPI.BROWSER_VERSION, '56.0.2924.87'),
          mlang.EQ(org.chromium.apis.web.BrowserAPI.OS_NAME, 'OSX'),
          mlang.EQ(org.chromium.apis.web.BrowserAPI.OS_VERSION, '10.12.2'),
          mlang.EQ(org.chromium.apis.web.BrowserAPI.INTERFACE_NAME,
            intface.interfaceName),
          mlang.EQ(org.chromium.apis.web.BrowserAPI.API_NAME, intface.apiName)
        )).select();
    });
    Promise.all(promises).then((results) => {
      results.forEach((defaultArrayDAO) => {
        expect(defaultArrayDAO.a.length).toBe(1);
      });
      done();
    });
  });
});
