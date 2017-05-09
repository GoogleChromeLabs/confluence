// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

foam.CLASS({
  name: 'ApiMatrix',
  package: 'org.chromium.apis.web',
  documentation: `ApiMatrix is a client side object that has methods
    to retrieve releaseDAO, interfaceDAO and their junction DAO, transform
    to CSV format string or JSON matrix that can be displayed on HTML as
    a nested table.`,
  requires: [
    'foam.dao.AnonymousSink',
    'foam.dao.ArraySink',
    'foam.dao.EasyDAO',
    'foam.mlang.ExpressionsSingleton',
    'org.chromium.apis.web.Release',
    'org.chromium.apis.web.ReleaseWebInterfaceJunction',
    'org.chromium.apis.web.WebInterface',
  ],
  properties: [
    {
      name: 'releaseApiDAO',
      documentation: 'A DAO containing Release <--> WebInterface relations.',
      typeName: 'DAO',
      required: true,
      final: true,
    },
    {
      name: 'releaseDAO',
      documentation: 'A DAO containing all known browser releases.',
      typeName: 'DAO',
      required: true,
      final: true,
    },
    {
      name: 'interfaceDAO',
      documentation: `A DAO containing all known <interface>.<property/method>
          API pairs.`,
      typeName: 'DAO',
      required: true,
      final: true,
    },
    {
      name: 'mlang',
      documentation: `Object contains mlang expressions, it supports
          logic operations such as AND and OR and math operations such
          as IN, EQ.

          TODO(markdittmer): Use "implements: ['foam.mlang.Expressions']" and
          "this.SOME_MLANG" instead.`,
      factory: function() {
        return this.ExpressionsSingleton.create();
      },
    },
  ],
  methods: [
    {
      name: 'getReleases',
      documentation: `Asynchronous fetch a group of "Release" objects from 
         "releaseDAO", grouped by browser name+version.`,
      returns: {
        typeName: 'JSON',
        documentation: '{<browserName>: <browserVersion>: <release>}}',
      },
      code: function() {
        return this.releaseDAO.select().then((arraySink) => {
          let releases = {};
          for (let i = 0; i < arraySink.a.length; i++) {
            let release = arraySink.a[i];
            let browserName = release.browserName;
            let browserVersion = release.browserVersion;
            if (!releases.hasOwnProperty(browserName)) {
              releases[browserName] = {};
            }
            if (!releases[browserName].hasOwnProperty(browserVersion)) {
              releases[browserName][browserVersion] = [];
            }
            releases[browserName][browserVersion].push(release);
          }
          return releases;
        });
      },
    },
    {
      name: 'toMatrix',
      documentation: `An asynchronous method that takes an array of browser
          release IDs and produces a nested matrix that captures
          Release <--> WebInterface relations. Data is gathered into this
          for ease of expression in client UI components.`,
      args: [
        {
          name: 'releaseKeys',
          typeName: 'StringArray',
          documentation: `Valid release keys used to filter releaseApiDAO.`,
        },
        {
          name: 'options',
          typeName: 'JSON',
          documentation: `An optional argument are used to filter matrix:
              searchKey: The result interface/API must contains this String
                value, ignore case.
              releaseOptions: An JSON of form {<releaseKey>: true, ...}.
                When set, the returned matrix are filtered so that it includes
                only APIs associated with a <releaseKey>.
              numAvailable: An integer, or integer array. When set, only
                APIs supported by exactly "numAvailable" releases are returned.
                When "numAvailable" is an array, any integer in it is a
                valid number supported releases.`,
        },
      ],
      returns: {
        typeName: 'JSON',
        documentation: `JSON of the form:
            {interfaceName: {<apiName>: {<ReleaseKey>: true, ...}...}...}...}
            This structure simplifies for displaying a nested table.`,
      },
      code: function(releaseKeys, options) {
        options = options || {};
        let matrix = {};
        let query = this.mlang.TRUE;
        if (options.searchKey) {
          query = this.mlang.AND(query, this.mlang.CONTAINS_IC(
            this.WebInterface.INTERFACE_KEY, options.searchKey));
        }
        return Promise.all(releaseKeys.map(
          (releaseId) => this.releaseDAO.find(releaseId).then((release) => {
            // Reject if this release does not exit in database.
            if (release === null) {
              return Promise.reject(Error(`${releaseId} does not exist.`));
            }
            // TODO(markdittmer): ContextualizingDAO should take care of this.
            return release.cls_.create(release, this.__subContext__)
              .interfaces.where(query).select(this.AnonymousSink.create({
                sink: {put: (_, iface) => {
                  let k0 = iface.interfaceName;
                  let k1 = iface.apiName;
                  let k2 = releaseId;
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
                }},
              }));
            })
        )).then(() => this.filterMatrix_(matrix, options));
      },
    },
    {
      name: 'filterMatrix_',
      documentation: `A post process of toMatrix, the matrix will be filtered
          based on options.releaseOptions and options.numAvailable.`,
      args: [
        {
          name: 'matrix',
          typeName: 'JSON',
          documentation: `JSON of the form:
              {<interfaceName>: {
                <apiName>: {<ReleaseKey>: true, ...}...}... }...}`,
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
            {interfaceName: {apiName: {ReleaseKey: true, ...} ...} ... } ...}
            This structure is easy for displaying a nested table.`,
      },
      code: function(matrix, options) {
        options = options || {};
        let releaseOptions = options.releaseOptions;
        let numAvailable = options.numAvailable;
        if (!releaseOptions && !numAvailable) return matrix;
        for (let webInterface in matrix) {
          if (!matrix.hasOwnProperty(webInterface)) continue;
          for (let api in matrix[webInterface]) {
            if (!Object.prototype.hasOwnProperty
                .call(matrix[webInterface], api)) {
              continue;
            }
            let releases = matrix[webInterface][api];
            if (this.filterApi_(releases, numAvailable, releaseOptions)) {
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
        filtered based on supported releases, numAvailable and release
        options.`,
      args: [
        {
          name: 'supportedReleases',
          documentation: 'A list of releases this API supports.',
        },
        {
          name: 'numAvailable',
          typeName: 'Int/Int Array',
          documentation: `The same optional argument as
            options.numAvailable in toMatrix().`,
        },
        {
          name: 'releaseOptions',
          typeName: 'JSON',
          documentation: `The same optional argument as
              options.releaseOptions in toMatrix().`,
        },
      ],
      returns: {
        typeName: 'Boolean',
        documentation: `True if this API does not match the filters
          and should not be displayed.`,
      },
      code: function(supportedReleases, numAvailable, releaseOptions) {
        if (numAvailable) {
          let numSupportedReleases = Object.keys(supportedReleases).length;
          if (!Array.isArray(numAvailable)) {
            numAvailable = [numAvailable];
          }
          if (numAvailable.indexOf(numSupportedReleases) === -1) {
            return true;
          }
        } // if (numAvailable)
        if (releaseOptions) {
          for (let releaseKey in releaseOptions) {
            if (!releaseOptions.hasOwnProperty(releaseKey)) continue;
            let available = supportedReleases.hasOwnProperty(releaseKey);
            if (available !== releaseOptions[releaseKey]) {
              return true;
            }
          }
        } // if (releaseOptions)
        return false;
      },
    },
    {
      name: 'toCSV',
      documentation: `Takes an array of release keys, produces a string of
        CSV format.`,
      args: [
        {
          name: 'releaseKeys',
          typeName: 'StringArray',
          documentation: `A string array contains valid release keys.`,
        },
      ],
      returns: {
        typeName: 'String',
        documentation: `Returns a string of CSV format.`,
      },
      code: function(releaseKeys) {
        return this.toMatrix(releaseKeys).then((matrix) =>
          this.matrixToCSV(releaseKeys, matrix));
      },
    },
    {
      name: 'matrixToCSV',
      documentation: `Takes an array of release keys, produces a string of
        CSV format.`,
      args: [
        {
          name: 'releaseKeys',
          typeName: 'StringArray',
          documentation: `A string array contains valid release keys.`,
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
      code: function(releaseKeys, matrix) {
        let table = [];
        // Add table header.
        table.push(['Interface', 'API'].concat(releaseKeys));
        let interfaces = Object.keys(matrix).sort();
        for (let i = 0; i < interfaces.length; i++) {
          let interfaceName = interfaces[i];
          let APIs = Object.keys(matrix[interfaceName]).sort();
          for (let j = 0; j < APIs.length; j++) {
            let apiName = APIs[j];
            let row = [interfaceName, apiName];
            for (let k = 0; k < releaseKeys.length; k++) {
              row.push(matrix[interfaceName][apiName]
                .hasOwnProperty(releaseKeys[k]));
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
      documentation: `Takes an array of release keys, produces a JSON
          contains number of proprietary APIs and falling behind APIs for
          each selected releases.`,
      args: [
        {
          name: 'releaseKeys',
          typeName: 'StringArray',
          documentation: `A string array contains valid release keys.`,
        },
      ],
      returns: {
        typeName: 'JSON',
        documentation: `A JSON of the form:
            {
              <releaseKey>:
              {
                total: An integer, the total number of APIs belongs to
                    this release.
                proprietary: An integer, number of APIs that only exists
                    in this release.
                fallBehind: An integer, number of APIs that exists in all
                    other selected releases but not this release.
              }
            }.`,
      },
      code: function(releaseKeys) {
        return this.releaseApiDAO
          .where(this.mlang.IN(
            this.ReleaseWebInterfaceJunction.SOURCE_ID, releaseKeys))
          .select(this.mlang.GROUP_BY(
            this.ReleaseWebInterfaceJunction.TARGET_ID,
            this.ArraySink.create()))
          .then((groups) => {
            let result = {};
            let totalReleases = releaseKeys.length;
            let interfaceKeys = groups.groupKeys;
            for (let i = 0; i < releaseKeys.length; i++) {
              result[releaseKeys[i]] = {
                total: 0,
                proprietary: 0,
                fallBehind: 0,
              };
            }
            for (let i = 0; i < interfaceKeys.length; i++) {
              let interfaceKey = interfaceKeys[i];
              let availableReleases = {};
              let releases = groups.groups[interfaceKey].a;
              for (let j = 0; j < releases.length; j++) {
                result[releases[j].sourceId].total++;
                availableReleases[releases[j].sourceId] = true;
                if (releases.length === 1) {
                  result[releases[j].sourceId].proprietary++;
                }
              }
              if (releases.length === totalReleases - 1) {
                releaseKeys.map((releaseKey) => {
                  if(!availableReleases.hasOwnProperty(releaseKey)) {
                    result[releaseKey].fallBehind++;
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
