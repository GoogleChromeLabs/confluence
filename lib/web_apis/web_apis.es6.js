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
require('./browser_api.es6');
require('./web_interface.es6');

foam.CLASS({
  name: 'WebAPIs',
  package: 'com.web.api',
  requires: [
    'com.web.api.Browser',
    'com.web.api.BrowserAPI',
    'com.web.api.WebInterface',
    'foam.dao.EasyDAO',
    'foam.mlang.sink.GroupBy',
    'foam.dao.ArrayDAO',
    'foam.mlang.ExpressionsSingleton',
  ],
  properties: [
    {
      name: 'browserAPIs',
      documentation: `A DAO that contains pairs of browser Id
        and interface Id.`,
      factory: function() {
        return this.EasyDAO.create({
          name: 'BrowserAPIDAO',
          of: this.BrowserAPI,
          daoType: 'MDAO',
        });
      },
    },
    {
      name: 'browsers',
      documentation: `A DAO that contains browser names, versions
        OS, OS versions and their browser Id.`,
      factory: function() {
        return this.EasyDAO.create({
          name: 'BrowserDAO',
          of: this.Browser,
          daoType: 'MDAO',
        });
      },
    },
    {
      name: 'interfaces',
      documentation: `A DAO that contains interface names, API names,
        and their interfaceId`,
      factory: function() {
        return this.EasyDAO.create({
          name: 'InterfaceDAO',
          of: this.WebInterface,
          daoType: 'MDAO',
        });
      },
    },
    {
      name: 'mlang',
      factory: function() {
        return this.ExpressionsSingleton.create();
      },
    },
  ],
  methods: [
    {
      name: 'importAPI',
      documentation: 'Import interface/API for a given version of browser.',
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
          name: 'OSName',
          documentation: `The name of operating system.`,
          typeName: 'String',
        },
        {
          name: 'OSVersion',
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
        OSName, OSVersion, apiCatalog) {
        let browser = this.Browser.create({
          browserName: browserName,
          browserVersion: browserVersion,
          OS: OSName,
          OSVersion: OSVersion,
        });
        let browserId = browser.id;
        this.browsers.put(browser);
        let interfaceNames = Object.keys(apiCatalog);
        for (let i = 0; i < interfaceNames.length; i += 1) {
          let interfaceName = interfaceNames[i];
          for (let j = 0; j < apiCatalog[interfaceName].length; j += 1) {
            let APIName = apiCatalog[interfaceName][j];
            let webInterface = this.WebInterface.create({
              interfaceName,
              APIName,
            });
            let interfaceId = webInterface.id;
            this.interfaces.put(webInterface);
            this.browserAPIs.put(this.BrowserAPI.create({
              browserId,
              interfaceId,
              id: `${browserId}.${interfaceId}`,
            }));
          }
        }
      },
    },
    {
      name: 'getBrowserKeys',
      documentation: `Asynch function to return an array of stored
        browsers' key. browserName and version are optional arguments
        to filter keys.`,
      args: [
        {
          name: 'browserName',
          documentation: `The optional argument that the returned browser
            key will match the given name`,
          typeName: 'String',
        },
        {
          name: 'version',
          documentation: `The optional argument that the returned browser
            key will match the given version`,
          typeName: 'String',
        },
      ],
      returns: {
        typeName: 'StringArray',
        documentation: `Return a string array contains matching browser keys`,
      },
      code: function(browserName, version) {
        return new Promise((resolve) => {
          this.browsers.select().then((arrayDAO) => {
            resolve(arrayDAO.a
            .map((browser) => browser.getBrowserKey())
            .filter((key) => {
              if (browserName &&
              key.split('_')[0].indexOf(browserName) !== 0) {
                return false;
              }
              if (version &&
              key.split('_')[1].indexOf(version) !== 0) {
                return false;
              }
              return true;
            }));
          });
        });
      },
    },
    {
      name: 'getBrowserId_',
      args: [
        {
          name: 'browserKey',
          typeName: 'String',
          documentation: `The string must be a key of browser`,
        },
      ],
      returns: {
        typeName: 'Number',
        documentation: `The id of this browser`,
      },
      code: function(browserKey) {
        let browserArr = browserKey.split('_');
        return this.Browser.create({
          browserName: browserArr[0],
          browserVersion: browserArr[1],
          OS: browserArr[2],
          OSVersion: browserArr[3],
        }).id;
      },
    },
    {
      name: 'toMap_',
      documentation: `Takes an array of browserAPIs and an array of
        browser keys, produce a JSON containing browser and interface
        relationship`,
      args: [
        {
          name: 'browserAPIs',
          typeName: 'BrowserAPI Array',
          documentation: `An array returned from DAO.select.`,
        },
        {
          name: 'brwoserKeys',
          typeName: 'StringArray',
          documentation: `A list of browser keys that contained in the
            browserAPIs`,
        },
      ],
      returns: {
        typeName: 'JSON',
        documentation: `Returns a JSON of the form:
              {interfaceName: {APIName: [Boolean ...]} ...}
            It contains information about browsers and their interface/API.`,
      },
      code: function(browserAPIs, browserKeys) {
        return new Promise(function(resolve) {
          let apiMap = {};
          let browserDict = {}; // O(1) lookup for browser's index in header.
          apiMap._header = browserKeys.slice();
          let numBrowsers = apiMap._header.length;
          // Fill in browserDict.
          for (let i = 0; i < numBrowsers; i += 1) {
            browserDict[this.getBrowserId_(browserKeys[i])] = i;
          }
          // Fill in apiMap.
          let promises = [];
          for (let i = 0; i < browserAPIs.length; i += 1) {
            let interfaceId = browserAPIs[i].interfaceId;
            promises.push(this.interfaces
              .where(this.mlang.EQ(this.WebInterface.ID, interfaceId)).select()
              .then((arrayDao) => {
                let interfaceName = arrayDao.a[0].interfaceName;
                let APIName = arrayDao.a[0].APIName;
                if (!apiMap[interfaceName] ||
                  typeof apiMap[interfaceName] !== 'object') {
                  // If typeof is not object, this may be a build-in function.
                  apiMap[interfaceName] = {};
                }
                if (!apiMap[interfaceName][APIName] ||
                  typeof apiMap[interfaceName][APIName] !== 'object') {
                  // If visit APIName for first time,
                  // initialize an array of false.
                  apiMap[interfaceName][APIName] = [];
                  for (let k = 0; k < numBrowsers; k += 1) {
                    apiMap[interfaceName][APIName].push(false);
                  }
                }
                apiMap[interfaceName][APIName][
                  browserDict[browserAPIs[i].browserId]] = true;
              }));
          }
          Promise.all(promises).then(function() {
            resolve(apiMap);
          });
        }.bind(this));
      },
    },
    {
      name: 'toMap',
      documentation: `Takes an array of browser keys, produce a JSON containing
        browser and interface relationship`,
      args: [
        {
          name: 'browserKeys',
          typeName: 'StringArray',
          documentation: `A string array contains valid browser keys`,
        },
      ],
      returns: {
        typeName: 'JSON',
        documentation: `Returns a JSON of the form:
                {interfaceName: {APIName: [Boolean ...]} ...}`,
      },
      code: function(browserKeys) {
        return new Promise(function(resolve) {
          let browserIds = browserKeys.map((key) => this.getBrowserId_(key));
          this.browserAPIs
            .where(this.mlang.IN(this.BrowserAPI.BROWSER_ID, browserIds))
            .select()
            .then((arrayDao) => {
              resolve(this.toMap_(arrayDao.a, browserKeys));
            });
        }.bind(this));
      },
    },
    {
      name: 'toCSV',
      documentation: `Takes an array of browser keys, produce a String of
        CSV format`,
      args: [
        {
          name: 'browserKeys',
          typeName: 'StringArray',
          documentation: `A string array contains valid browser keys`,
        },
      ],
      returns: {
        typeName: 'String',
        documentation: `Returns a string of csv format`,
      },
      code: function(browserKeys) {
        return new Promise(function(resolve) {
          this.toMap(browserKeys).then(function(result) {
            let table = [];
            // Add table header.
            table.push(['Interface', 'API'].concat(result._header));
            let interfaces = Object.keys(result);
            for (let i = 0; i < interfaces.length; i += 1) {
              let interfaceName = interfaces[i];
              if (interfaceName === '_header') continue;
              let APIs = Object.keys(result[interfaceName]);
              for (let j = 0; j < APIs.length; j += 1) {
                let APIName = APIs[j];
                table.push([interfaceName, APIName]
                  .concat(result[interfaceName][APIName]));
              }
            }
            let csv = '';
            for (let i = 0; i < table.length; i += 1) {
              csv += table[i].join(',');
              csv += '\n';
            }
            resolve(csv);
          });
        }.bind(this));
      },
    },
  ],
});
