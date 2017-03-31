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
        let matrix = {};
        return Promise.all(browserKeys.map(
          (browserId) => this.browserDAO.find(browserId)
            .then((browser) => {
              // Reject if this browser does not exit in database.
              if (browser === null) {
                return Promise.reject(Error(`${browserId} does not exist.`));
              }
              // TODO(markdittmer): ContextualizingDAO should take care of this.
              return browser.cls_.create(browser, this.__subContext__)
                  .interfaces.select({
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
                      // hasOwnProperty could also be a apiName,
                      // thus use another Object's hasOwnProperty
                      // and call it with 'this' set to foo
                      if (!({}).hasOwnProperty.call(matrix[k0], k1)) {
                        matrix[k0][k1] = {};
                      }
                      matrix[k0][k1][k2] = true;
                    },
                    eof: function() {},
                  });
                })
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
          let interfaces = Object.keys(result).sort();
          for (let i = 0; i < interfaces.length; i++) {
            let interfaceName = interfaces[i];
            let APIs = Object.keys(result[interfaceName]).sort();
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
