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
    to retrieve browserDAO, interfaceDAO and their junction DAO, transform
    to CSV format string or JSON matrix that can be displayed on HTML as
    a nested table.`,
  requires: [
    'org.chromium.apis.web.Browser',
    'org.chromium.apis.web.WebInterface',
    'org.chromium.apis.web.BrowserWebInterfaceJunction',
    'foam.dao.EasyDAO',
    'foam.dao.ArraySink',
    'foam.mlang.ExpressionsSingleton',
  ],
  properties: [
    {
      name: 'browserApiDAO',
      documentation: `A DAO containing junction objects of Browser and
        WebInterface.`,
      typeName: 'BrowserAPI DAO',
      required: true,
      final: true,
    },
    {
      name: 'browserDAO',
      documentation: `A DAO containing all browser infomation.`,
      typeName: 'Browser DAO',
      required: true,
      final: true,
    },
    {
      name: 'interfaceDAO',
      documentation: `A DAO containing all interface and API pairs.`,
      typeName: 'WebInterface DAO',
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
          documentation: `Valid browser keys used to filter browserApiDAO.`,
        },
      ],
      returns: {
        typeName: 'JSON',
        documentation: `JSON of the form:
            {interfaceName: {apiName: {BrowserKey: true, ...} ...} ... } ...}
            This structure is easy for displaying a nested table.`,
      },
      code: function(browserKeys) {
        var matrix = {};
        return Promise.all(browserKeys.map(
          browserId => this.browserDAO.find(browserId)
            .then(browser => {this; return browser.interface.select({
                put: iface => {
                  var k0 = browserId;
                  var k1 = iface.interfaceName;
                  var k2 = iface.interfaceKey;
                  var m0 = matrix[k0] = matrix[k0] || {};
                  var m1 = m0[k1] = m0[k1] || {};
                  m1[k2] = true;
                },
                eof: function() {},
            })})
        )).then(() => matrix);
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
