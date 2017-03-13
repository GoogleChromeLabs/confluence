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
  name: 'WebAPIs',
  package: 'org.chromium.apis.web',
  documentation: `WebAPIs is the main model of this project. It has a
    property called browserAPIs which is a FOAM DAO. It contains interface and
    API information for major browsers and version for recent two years. It
    has methods to transform the data model from Google Cloud Datastore
    to CSV format String or JSON structure to be displayed in client side.
    It also has methods to do data analysis.`,
  requires: [
    'org.chromium.apis.web.BrowserAPI',
    'foam.dao.EasyDAO',
    'foam.dao.ArrayDAO',
    'foam.mlang.ExpressionsSingleton',
    'foam.mlang.sink.GroupBy',
    'foam.mlang.sink.Count',
  ],
  properties: [
    {
      name: 'browserAPIs',
      documentation: `This should be a server GCD DAO that contains
        interfaece, API information for major browsers and versions
        for recent 2 years. It is implemented as a MDAO for now.`,
      factory: function() {
        return this.EasyDAO.create({
          name: 'BrowserAPIDAO',
          of: this.BrowserAPI,
          daoType: 'MDAO',
        }).addPropertyIndex(this.BrowserAPI.BROWSER_KEY)
        .addPropertyIndex(this.BrowserAPI.INTERFACE_KEY);
      },
    },
    {
      name: 'mlang',
      documentation: `FOAM's mlang expressiongs, it supports Math and
        logic operations`,
      factory: function() {
        return this.ExpressionsSingleton.create();
      },
    },
  ],
  methods: [
    {
      name: 'importAPI',
      documentation: `Anynchronous function to import interface/API for a
        given version of browser. The function will do nothing if the given
        browser already exists in browserAPIs`,
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
        return this.browserAPIs.select(this.GroupBy.create({
          arg1: this.BrowserAPI.BROWSER_KEY,
          arg2: this.Count.create(),
        })).then((browserKeyGroups) => {
          // Exits early if given browser already exists in browserAPIs.
          if (browserKeyGroups.groupKeys.indexOf(browserKey) >= 0) {
            return false;
          }
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
          return true;
        });
      },
    },
    {
      name: 'toMap_',
      documentation: `A method that takes an array of browserAPIs and an
        array of browser keys, produce a JSON containing browser and
        interface relationship.`,
      args: [
        {
          name: 'browserAPIs',
          typeName: 'BrowserAPI Array',
          documentation: `An array returned from DAO.select.`,
        },
        {
          name: 'browserKeys',
          typeName: 'StringArray',
          documentation: `A list of browser keys that contained in the
            browserAPIs.`,
        },
      ],
      returns: {
        typeName: 'JSON',
        documentation: `Returns a JSON of the form:
            {interfaceName: {apiName: [Boolean ...]} ...}
            It contains information about browsers and their interface/API.`,
      },
      code: function(browserAPIs, browserKeys) {
        let apiMap = {};
        let browserDict = {}; // O(1) lookup for browser's index in header.
        apiMap._header = browserKeys.slice();
        let numBrowsers = apiMap._header.length;
        // Fill in browserDict.
        for (let i = 0; i < numBrowsers; i++) {
          browserDict[browserKeys[i]] = i;
        }
        // Fill in apiMap.
        for (let i = 0; i < browserAPIs.length; i++) {
          let interfaceName = browserAPIs[i].interfaceName;
          let apiName = browserAPIs[i].apiName;
          if (!apiMap[interfaceName] ||
            typeof apiMap[interfaceName] !== 'object') {
            // If typeof is not object, this may be a build-in function,
            // overwrite it.
            apiMap[interfaceName] = {};
          }
          if (!apiMap[interfaceName][apiName] ||
            typeof apiMap[interfaceName][apiName] !== 'object') {
            // If visit apiName for first time,
            // initialize an array of false.
            apiMap[interfaceName][apiName] = [];
            for (let k = 0; k < numBrowsers; k++) {
              apiMap[interfaceName][apiName].push(false);
            }
          }
          apiMap[interfaceName][apiName][browserDict[
            browserAPIs[i].browserKey]] = true;
        }
        return(apiMap);
      },
    },
    {
      name: 'toMap',
      documentation: `An asynchronous method that takes an array of browser
        keys, produce a JSON containing browser and interface relationship.
        This structure should be easy to be displayed as a nested table in
        client side.`,
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
                {interfaceName: {apiName: [Boolean ...]} ...}
                This structure is easy for displaying a nested table.`,
      },
      code: function(browserKeys) {
        return this.browserAPIs
          .where(this.mlang.IN(this.BrowserAPI.BROWSER_KEY, browserKeys))
          .select()
          .then((arraySink) => {
            return(this.toMap_(arraySink.a, browserKeys));
          });
      },
    },
    {
      name: 'toCSV',
      documentation: `Takes an array of browser keys, produce a string of
        CSV format.`,
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
        return this.toMap(browserKeys).then(function(result) {
          let table = [];
          // Add table header.
          table.push(['Interface', 'API'].concat(result._header));
          let interfaces = Object.keys(result);
          for (let i = 0; i < interfaces.length; i++) {
            let interfaceName = interfaces[i];
            if (interfaceName === '_header') continue;
            let APIs = Object.keys(result[interfaceName]);
            for (let j = 0; j < APIs.length; j++) {
              let apiName = APIs[j];
              table.push([interfaceName, apiName]
                .concat(result[interfaceName][apiName]));
            }
          }
          let csv = '';
          for (let i = 0; i < table.length; i++) {
            csv += table[i].join(',');
            csv += '\n';
          }
          return(csv);
        });
      },
    },
  ],
});
