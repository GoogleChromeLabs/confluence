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

require('./browser_api.es6');

foam.CLASS({
  name: 'WebAPIs_oneTable',
  package: 'com.web.api',
  requires: [
    'com.web.api.BrowserAPI_oneTable',
    'foam.dao.EasyDAO',
    'foam.mlang.sink.GroupBy',
    'foam.dao.ArrayDAO',
    'foam.mlang.ExpressionsSingleton',
  ],
  properties: [
    {
      name: 'browserAPIs',
      documentation: `This should be a server GDS DAO that contains
        interfaec API information for major browsers of recent 2 years'
        version`,
      factory: function() {
        return this.EasyDAO.create({
          name: 'BrowserAPIDAO',
          of: this.BrowserAPI_oneTable,
          daoType: 'MDAO',
        }).addPropertyIndex(this.BrowserAPI_oneTable.BROWSER_KEY);
      },
    },
    {
      name: 'mlang',
      factory: function() {
        return this.ExpressionsSingleton.create();
      },
    },
    {
      name: 'browserList',
      documentation: `An string array of browser keys.`,
      class: 'StringArray',
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
        let browserKey = `${browserName}_${browserVersion}_${osName}_${osVersion}`;
        if (this.browserList.indexOf(browserKey) >= 0) return;
        this.browserList.push(browserKey);
        let interfaceNames = Object.keys(apiCatalog);
        for (let i = 0; i < interfaceNames.length; i += 1) {
          let interfaceName = interfaceNames[i];
          for (let j = 0; j < apiCatalog[interfaceName].length; j += 1) {
            let apiName = apiCatalog[interfaceName][j];
            this.browserAPIs.put(this.BrowserAPI_oneTable.create({
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
        let apiMap = {};
        let browserDict = {}; // O(1) lookup for browser's index in header.
        apiMap._header = browserKeys.slice();
        let numBrowsers = apiMap._header.length;
        // Fill in browserDict.
        for (let i = 0; i < numBrowsers; i += 1) {
          browserDict[browserKeys[i]] = i;
        }
        // Fill in apiMap.
        for (let i = 0; i < browserAPIs.length; i += 1) {
          let interfaceName = browserAPIs[i].interfaceName;
          let apiName = browserAPIs[i].apiName;
          if (!apiMap[interfaceName] ||
            typeof apiMap[interfaceName] !== 'object') {
            // If typeof is not object, this may be a build-in function.
            apiMap[interfaceName] = {};
          }
          if (!apiMap[interfaceName][apiName] ||
            typeof apiMap[interfaceName][apiName] !== 'object') {
            // If visit apiName for first time,
            // initialize an array of false.
            apiMap[interfaceName][apiName] = [];
            for (let k = 0; k < numBrowsers; k += 1) {
              apiMap[interfaceName][apiName].push(false);
            }
          }
          apiMap[interfaceName][apiName][browserDict[
            browserAPIs[i].browserKey]] = true;
        }
        console.timeEnd('one-table-process');
        return(apiMap);
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
        console.time('one-table-query');
        return this.browserAPIs
          .where(this.mlang.IN(this.BrowserAPI_oneTable.BROWSER_KEY, browserKeys))
          .select()
          .then((arraySink) => {
            console.timeEnd('one-table-query');
            console.time('one-table-process');
            return(this.toMap_(arraySink.a, browserKeys));
          });
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
          documentation: `A string array contains valid browser keys.`,
        },
      ],
      returns: {
        typeName: 'String',
        documentation: `Returns a string of csv format.`,
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
