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

foam.CLASS({
  name: 'ApiImporter',
  package: 'org.chromium.apis.web',
  documentation: `API Importer is a class that handles importing browserAPIs
    from webCatalog object to browserAPI DAO.`,
  requires: [
    'org.chromium.apis.web.BrowserAPI',
    'foam.mlang.ExpressionsSingleton',
    'foam.dao.EasyDAO',
  ],
  properties: [
    {
      name: 'browserAPIs',
      documentation: `This should be a server GCD DAO that contains
        interface, API information for major browsers and versions
        for recent 2 years. It is implemented as a MDAO for now.`,
      factory: function() {
        let browserApiDao = this.EasyDAO.create({
          name: 'BrowserAPIDAO',
          of: this.BrowserAPI,
          daoType: 'MDAO',
        });
        browserApiDao.addPropertyIndex(this.BrowserAPI.BROWSER_KEY);
        browserApiDao.addPropertyIndex(this.BrowserAPI.INTERFACE_KEY);
        return browserApiDao;
      },
    },
  ],
  methods: [
    {
      name: 'import',
      documentation: `A synchronous function to import interface/API from
        web catalog for a given version of browser.`,
      args: [
        {
          name: 'browserName',
          documentation: `The name of imported browser.`,
          typeName: 'String',
        },
        {
          name: 'browserVersion',
          documentation: `The version of imported browser.`,
          typeName: 'String',
        },
        {
          name: 'osName',
          documentation: `The name of operating system.`,
          typeName: 'String',
        },
        {
          name: 'osVersion',
          documentation: `The version of operating system.`,
          typeName: 'String',
        },
        {
          name: 'apiCatalog',
          documentation: `The web catalog of this version of
            browser, tested on the given operating system and version.
            The json object should be the JSON object returned from
            com.web.catalog.apiExtractor`,
          typeName: 'JSON',
        },
      ],
      code: function(browserName, browserVersion,
        osName, osVersion, apiCatalog) {
        let interfaceNames = Object.keys(apiCatalog);
        for (let i = 0; i < interfaceNames.length; i++) {
          let interfaceName = interfaceNames[i];
          for (let j = 0; j < apiCatalog[interfaceName].length; j++) {
            let apiName = apiCatalog[interfaceName][j];
            this.browserAPIs.put(this.BrowserAPI.create({
              browserName,
              browserVersion,
              osName,
              osVersion,
              interfaceName,
              apiName,
            }));
          }
        }
      },
    },
  ],
});
