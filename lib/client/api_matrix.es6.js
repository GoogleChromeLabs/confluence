/**
 * @license
 * Copyright 2017 Google Inc. All Rights Reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

foam.CLASS({
  name: 'ApiMatrix',
  package: 'org.chromium.apis.web',
  documentation: `ApiMatrix is a client side object that has methods
    to retrieve browserAPIs from server DAO, transform browserAPIs to
    CSV format string, tranform browserAPIs to object that can be
    displayed on HTML as a nested table.`,
  requires: [
    'foam.dao.EasyDAO',
    'foam.dao.ArraySink',
    'foam.mlang.ExpressionsSingleton',
    'org.chromium.apis.web.BrowserAPI',
  ],
  properties: [
    {
      name: 'browserAPIs',
      documentation: `The WebAPIs object where the browserAPIs will be
        be fetched from.`,
      typeName: 'BrowserAPI DAO',
      required: true,
      final: true,
    },
    {
      name: 'mlang',
      documentation: `Object contains mlang expressions, it supports
        logic operations such as AND and OR and math operations such
        as IN, EQ.`,
      factory: function() {
        return this.ExpressionsSingleton.create();
      },
    },
  ],
  methods: [
    {
      name: 'toMatrix',
      documentation: `An asynchronous method that takes an array of browser
        keys, produces a nested matrix that contains browser and interface
        relationship. This structure should be easy to be displayed as a
        nested table in client side.`,
      args: [
        {
          name: 'browserKeys',
          typeName: 'StringArray',
          documentation: `Valid browser keys used to filter browserAPIs.`,
        },
      ],
      returns: {
        typeName: 'JSON',
        documentation: `JSON of the form:
            {interfaceName: {apiName: {BrowserKey: true, ...} ...} ... } ...}
            This structure is easy for displaying a nested table.`,
      },
      code: function(browserKeys) {
        return this.browserAPIs
          .where(this.mlang.IN(this.BrowserAPI.BROWSER_KEY, browserKeys))
          .orderBy(this.BrowserAPI.INTERFACE_KEY)
          .select(this.mlang.GROUP_BY(this.BrowserAPI.INTERFACE_KEY,
            this.ArraySink.create()))
          .then((groups) => {
            let matrix = {};
            let interfaceApis = groups.groupKeys;
            for (let i = 0; i < interfaceApis.length; i++) {
              let browserAPIs = groups.groups[interfaceApis[i]].a;
              let interfaceName = browserAPIs[0].interfaceName;
              let apiName = browserAPIs[0].apiName;
              if (!matrix.hasOwnProperty(interfaceName)) {
                matrix[interfaceName] = {};
              }
              matrix[interfaceName][apiName] = {};
              for (let j = 0; j < browserAPIs.length; j++) {
                matrix[interfaceName][apiName][
                  browserAPIs[j].browserKey] = true;
              }
            }
            return matrix;
          });
      },
    },
    {
      name: 'toCSV',
      documentation: `Takes an array of browser keys, produces a string of
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
        return this.toMatrix(browserKeys).then(function(result) {
          let table = [];
          // Add table header.
          table.push(['Interface', 'API'].concat(browserKeys));
          let interfaces = Object.keys(result);
          for (let i = 0; i < interfaces.length; i++) {
            let interfaceName = interfaces[i];
            let APIs = Object.keys(result[interfaceName]);
            for (let j = 0; j < APIs.length; j++) {
              let apiName = APIs[j];
              let row = [interfaceName, apiName];
              for (let k = 0; k < browserKeys.length; k++) {
                row.push(result[interfaceName][apiName]
                  .hasOwnProperty(browserKeys[k]));
              }
              table.push(row);
            }
          }
          let csv = '';
          for (let i = 0; i < table.length; i++) {
            csv += table[i].join(',');
            csv += '\n';
          }
          return csv;
        });
      },
    },
  ],
});
