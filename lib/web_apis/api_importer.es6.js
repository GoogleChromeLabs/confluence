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

require('./browser.es6');
require('./web_interface.es6');
require('./version_history.es6');

foam.CLASS({
  name: 'ApiImporter',
  package: 'org.chromium.apis.web',
  documentation: `API Importer is a class that handles importing browserAPIs
    from webCatalog object to browserAPI DAO.`,
  requires: [
    'org.chromium.apis.web.Browser',
    'org.chromium.apis.web.WebInterface',
    'org.chromium.apis.web.BrowserWebInterfaceJunction',
    'org.chromium.apis.web.VersionHistory',
    'foam.mlang.ExpressionsSingleton',
    'foam.dao.EasyDAO',
  ],
  exports: [
    'browserDAO',
    'interfaceDAO as webInterfaceDAO',
    'browserApiDAO as browserWebInterfaceJunctionDAO',
  ],
  properties: [
    {
      name: 'browserApiDAO',
      documentation: `A DAO that contains pairs of browser
        and web interface.`,
      factory: function() {
        return this.EasyDAO.create({
          name: 'BrowserWebInterfaceJunctionDAO',
          of: this.BrowserWebInterfaceJunction,
          daoType: 'MDAO',
        });
      },
    },
    {
      name: 'browserDAO',
      documentation: `A DAO that contains browser names, versions
        OS, OS versions and its browser key.`,
      factory: function() {
        return this.EasyDAO.create({
          name: 'browserDAO',
          of: this.Browser,
          daoType: 'MDAO',
        });
      },
    },
    {
      name: 'interfaceDAO',
      documentation: `A DAO that contains interface names, API names,
        and its interface key`,
      factory: function() {
        return this.EasyDAO.create({
          name: 'interfaceDAO',
          of: this.WebInterface,
          daoType: 'MDAO',
        });
      },
    },
    {
      name: 'versionHistory',
      documentation: `An helper gets release date for a brwoser version.`,
      factory: function() {
        return this.VersionHistory.create({
          browserHistory: require('../../data/version_history.json'),
        });
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
          let browser = this.Browser.create({
            browserName: browserName,
            browserVersion: browserVersion,
            osName: osName,
            osVersion: osVersion,
            releaseDate: this.versionHistory
              .getReleaseDate(browserName, browserVersion),
          });
          this.browserDAO.put(browser);
          let interfaceNames = Object.keys(apiCatalog);
          for (let i = 0; i < interfaceNames.length; i += 1) {
            let interfaceName = interfaceNames[i];
            for (let j = 0; j < apiCatalog[interfaceName].length; j += 1) {
              let apiName = apiCatalog[interfaceName][j];
              let webInterface = this.WebInterface.create({
                interfaceName,
                apiName,
              });
              this.interfaceDAO.put(webInterface);
              webInterface.browser.put(browser);
            }
          }
      },
    },
  ],
});
