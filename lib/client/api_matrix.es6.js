// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

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
      name: 'getBrowsers',
      documentation: `An asynchronous method returns a group of browser
        objects, the groups are grouped by browser name and versions.`,
      returns: {
        typeName: 'group',
        documentation: `browsers group by browser name and version.`,
      },
      code: function() {
        return this.browserDAO.select(this.mlang.GROUP_BY(
          this.Browser.BROWSER_NAME,
          this.mlang.GROUP_BY(
            this.Browser.BROWSER_VERSION,
            this.ArraySink.create())
        )).then((group) => {
          let browsers = {};
          let browserNames = group.groupKeys;
          for (let i = 0; i < browserNames.length; i++) {
             browsers[browserNames[i]] = {};
             let browserVersions = group.groups[browserNames[i]].groupKeys;
             for (let j = 0; j < browserVersions.length; j++) {
              browsers[browserNames[i]][browserVersions[j]] =
                group.groups[browserNames[i]].groups[[browserVersions[j]]].a;
             }
          }
          return browsers;
        });
      },
    },
    {
      name: 'filterMatrix_',
      documentation: `A post process of toMatrix, the result api toMatrix
        will be filtered based on options.browserOptions and options.
        numAvailable.`,
      args: [
        {
          name: 'matrix',
          typeName: 'JSON',
          documentation: `JSON of the form:
            {interfaceName: {apiName: {BrowserKey: true, ...} ...} ... } ...}`,
        },
        {
          name: 'options',
          typeName: 'JSON',
          documentation: `An optional argument are used to filter the
            matrix result:
              searchKey: the result interface/API must contains this String
                value, ignore cases.
              browserOptions: An JSON of form {browserKey: boolean, ...}.
                The returned result are filtered. So that only APIs that
                whether it is available in this browser equals to
                options[browser] are returned.
                This option can be used to do queries such as finding
                proprietary apis for a browser in a set of browsers.
              numAvailable: an interger or interger Array that the web
                interface whose number of available browser not included
                in this array or not equal to this interger are filtered.
                This option can be used to do queries such as finding apis
                that exitsin at lease 3 browser vendors.`,
        },
      ],
      returns: {
        typeName: 'JSON',
        documentation: `JSON of the form:
            {interfaceName: {apiName: {BrowserKey: true, ...} ...} ... } ...}
            This structure is easy for displaying a nested table.`,
      },
      code: function(matrix, options) {
        options = options || {};
        let browserOptions = options.browserOptions;
        let numAvailable = options.numAvailable;
        if (!browserOptions && !numAvailable) return matrix;
        for (let webInterface in matrix) {
          if (!matrix.hasOwnProperty(webInterface)) continue;
          for (let api in matrix[webInterface]) {
            if (!Object.prototype.hasOwnProperty
              .call(matrix[webInterface], api)) continue;
            if (numAvailable) {
              if ((!Array.isArray(numAvailable) && numAvailable !==
                Object.keys(matrix[webInterface][api]).length) ||
                numAvailable.indexOf(Object.keys(matrix[webInterface][api])
                .length) === -1) {
                  delete matrix[webInterface][api];
                  continue;
                }
            }
            if (browserOptions) {
              for(let browserKey in browserOptions) {
                if (!browserOptions.hasOwnProperty(browserKey)) continue;
                if (matrix[webInterface][api].hasOwnProperty(browserKey) !==
                  browserOptions[browserKey]) {
                    delete matrix[webInterface][api];
                    break;
                }
              }
            }
          }
          if (Object.keys(matrix[webInterface]).length === 0) {
            delete matrix[webInterface];
          }
        }
        return matrix;
      },
    },
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
        {
          name: 'options',
          typeName: 'JSON',
          documentation: `An optional argument are used to filter the
            matrix result:
              searchKey: the result interface/API must contains this String
                value, ignore cases.
              browserOptions: An JSON of form {browserKey: boolean, ...}.
                The returned result are filtered. So that only APIs that
                whether it is available in this browser equals to
                options[browser] are returned.
                This option can be used to do queries such as finding
                proprietary apis for a browser in a set of browsers.
              numAvailable: an interger or interger Array that the web
                interface whose number of available browser not included
                in this array or not equal to this interger are filtered.
                This option can be used to do queries such as finding apis
                that exitsin at lease 3 browser vendors.`,
        },
      ],
      returns: {
        typeName: 'JSON',
        documentation: `JSON of the form:
            {interfaceName: {apiName: {BrowserKey: true, ...} ...} ... } ...}
            This structure is easy for displaying a nested table.`,
      },
      code: function(browserKeys, options) {
        options = options || {};
        let matrix = {};
        let query = this.mlang.TRUE;
        if (options.searchKey) {
          query = this.mlang.AND(query, this.mlang.CONTAINS_IC(
            this.WebInterface.INTERFACE_KEY, options.searchKey));
        }
        return Promise.all(browserKeys.map(
          (browserId) => this.browserDAO.find(browserId).then((browser) => {
            // Reject if this browser does not exit in database.
            if (browser === null) {
              return Promise.reject(Error(`${browserId} does not exist.`));
            }
            // TODO(markdittmer): ContextualizingDAO should take care of this.
            return browser.cls_.create(browser, this.__subContext__)
              .interfaces.where(query).select({
                put: (iface) => {
                  let k0 = iface.interfaceName;
                  let k1 = iface.apiName;
                  let k2 = browserId;
                  // Function and Object's built-in methods can be
                  // interface name or api name.
                  // So matrix[k0] is not sufficient to check if
                  // k0 exits in matrix, use hasOwnProperty instead.
                  if (!matrix.hasOwnProperty(k0)) {
                    matrix[k0] = {};
                  }
                  // "hasOwnProperty" could also be a apiName,
                  // thus use Object.prototype.hasOwnProperty
                  // to ensure we invoke the right function.
                  if (!Object.prototype.hasOwnProperty
                    .call(matrix[k0], k1)) {
                      matrix[k0][k1] = {};
                    }
                  matrix[k0][k1][k2] = true;
                },
                eof: function() {},
              });
            })
        )).then(() => this.filterMatrix_(matrix, options));
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
        return this.toMatrix(browserKeys).then((matrix) =>
          this.matrixToCSV(browserKeys, matrix));
      },
    },
    {
      name: 'matrixToCSV',
      documentation: `Takes an array of browser keys, produces a string of
        CSV format.`,
      args: [
        {
          name: 'browserKeys',
          typeName: 'StringArray',
          documentation: `A string array contains valid browser keys.`,
        },
        {
          name: 'matrix',
          typeName: 'JSON',
          documentation: `The JSON object returned from toMatrix.`,
        },
      ],
      returns: {
        typeName: 'String',
        documentation: `Returns a string of csv format.`,
      },
      code: function(browserKeys, matrix) {
        let table = [];
        // Add table header.
        table.push(['Interface', 'API'].concat(browserKeys));
        let interfaces = Object.keys(matrix).sort();
        for (let i = 0; i < interfaces.length; i++) {
          let interfaceName = interfaces[i];
          let APIs = Object.keys(matrix[interfaceName]).sort();
          for (let j = 0; j < APIs.length; j++) {
            let apiName = APIs[j];
            let row = [interfaceName, apiName];
            for (let k = 0; k < browserKeys.length; k++) {
              row.push(matrix[interfaceName][apiName]
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
      },
    },
    {
      name: 'getAnalytics',
      documentation: `Takes an array of browser keys, produces a JSON
        contains number of proprietary APIs and falling behind APIs for
        each selected browsers.`,
      args: [
        {
          name: 'browserKeys',
          typeName: 'StringArray',
          documentation: `A string array contains valid browser keys.`,
        },
      ],
      returns: {
        typeName: 'JSON',
        documentation: `A JSON of form:
          {
            browserKey:
            {
              total: Int,
              proprietary: Int,
              fallBehind: Int
            }
          }.`,
      },
      code: function(browserKeys) {
        return this.browserApiDAO
          .where(this.mlang
            .IN(this.BrowserWebInterfaceJunction.SOURCE_ID, browserKeys))
          .select(this.mlang.GROUP_BY(
            this.BrowserWebInterfaceJunction.TARGET_ID,
            this.ArraySink.create()))
          .then((groups) => {
            let result = {};
            let totalBrowsers = browserKeys.length;
            let interfaceKeys = groups.groupKeys;
            for (let i = 0; i < browserKeys.length; i++) {
              result[browserKeys[i]] = {
                // Total is the total number of APIs belongs to
                // this browser.
                total: 0,
                // Proprietary is the number of APIs that
                // only exists in this browser.
                proprietary: 0,
                // FallBehind is the number of APIs that exists
                // in all other selected browsers but not this browser.
                fallBehind: 0,
              };
            }
            for (let i = 0; i < interfaceKeys.length; i++) {
              let interfaceKey = interfaceKeys[i];
              let availableBrowsers = {};
              let browsers = groups.groups[interfaceKey].a;
              for (let j = 0; j < browsers.length; j++) {
                result[browsers[j].sourceId].total++;
                availableBrowsers[browsers[j].sourceId] = true;
                if (browsers.length === 1) {
                  result[browsers[j].sourceId].proprietary++;
                }
              }
              if (browsers.length === totalBrowsers - 1) {
                browserKeys.map((browserKey) => {
                  if(!availableBrowsers.hasOwnProperty(browserKey)) {
                    result[browserKey].fallBehind++;
                  }
                });
              }
            }
            return result;
          });
      },
    },
  ],
});
