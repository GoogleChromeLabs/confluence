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
        return this.browserDAO.select().then((arraySink) => {
          let browsers = {};
          for (let i = 0; i < arraySink.a.length; i++) {
            let browser = arraySink.a[i];
            let browserName = browser.browserName;
            let browserVersion = browser.browserVersion;
            if (!browsers.hasOwnProperty(browserName)) {
              browsers[browserName] = {};
            }
            if (!browsers[browserName].hasOwnProperty(browserVersion)) {
              browsers[browserName][browserVersion] = [];
            }
            browsers[browserName][browserVersion].push(browser);
          }
          return browsers;
        });
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
              searchKey: The result interface/API must contains this String
                value, ignore cases.
              browserOptions: An JSON of form {browserKey: boolean, ...}.
                When set, the returned matrix are filtered so that it only
                includes APIs that its supported browser has true value in
                browser option and its unsupported browser has false value
                in option.
              numAvailable: An integer, or integer array. When set, only
                APIs supported by numAvailable browsers are returned.
                When numAvailable is an array, any integer in it is a
                valid number of supporting browsers.`,
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
                  // interface name or API name.
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
      name: 'filterMatrix_',
      documentation: `A post process of toMatrix, the matrix will be filtered
        based on options.browserOptions and options.numAvailable.`,
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
          documentation: `The same optional argument used to filter the
            matrix result in toMatrix().`,
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
                .call(matrix[webInterface], api)) {
              continue;
            }
            let browsers = matrix[webInterface][api];
            if (this.filterApi_(browsers, numAvailable, browserOptions)) {
              delete matrix[webInterface][api];
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
      name: 'filterApi_',
      documentation: `A helper function to check whether the API should be
        filtered based on supported browsers, numAvailable and browser
        options.`,
      args: [
        {
          name: 'supportedBrowsers',
          documentation: `A list of browsers this API supports.`,
        },
        {
          name: 'numAvailable',
          typeName: 'Int/Int Array',
          documentation: `The same optional argument as
            options.numAvailable in toMatrix().`,
        },
        {
          name: 'browserOptions',
          typeName: 'JSON',
          documentation: `The same optional argument as
            options.browserOptions in toMatrix().`,
        },
      ],
      returns: {
        typeName: 'Boolean',
        documentation: `True if this API does not matrch the filters
          and should not be displayed.`,
      },
      code: function(supportedBrowsers, numAvailable, browserOptions) {
        if (numAvailable) {
          let numSupportedBrowsers = Object.keys(supportedBrowsers).length;
          if (!Array.isArray(numAvailable)) {
            numAvailable = [numAvailable];
          }
          if (numAvailable.indexOf(numSupportedBrowsers) === -1) {
            return true;
          }
        } // if (numAvailable)
        if (browserOptions) {
          for (let browserKey in browserOptions) {
            if (!browserOptions.hasOwnProperty(browserKey)) continue;
            let available = supportedBrowsers.hasOwnProperty(browserKey);
            if (available !== browserOptions[browserKey]) {
              return true;
            }
          }
        } // if (browserOptions)
        return false;
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
        documentation: `Returns a string of CSV format.`,
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
        documentation: `Returns a string of CSV format.`,
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
        documentation: `A JSON of the form:
          {
            browserKey:
            {
              total: An integer, the total number of APIs belongs to
                  this browser.
              proprietary: An integer, number of APIs that only exists
                  in this browser.
              fallBehind: An integer, number of APIs that exists in all
                  other selected browsers but not this browser.
            }
          }.`,
      },
      code: function(browserKeys) {
        return this.browserApiDAO
          .where(this.mlang.IN(
            this.BrowserWebInterfaceJunction.SOURCE_ID, browserKeys))
          .select(this.mlang.GROUP_BY(
            this.BrowserWebInterfaceJunction.TARGET_ID,
            this.ArraySink.create()))
          .then((groups) => {
            let result = {};
            let totalBrowsers = browserKeys.length;
            let interfaceKeys = groups.groupKeys;
            for (let i = 0; i < browserKeys.length; i++) {
              result[browserKeys[i]] = {
                total: 0,
                proprietary: 0,
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
