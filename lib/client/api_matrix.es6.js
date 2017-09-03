// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

require('../web_apis/web_interface.es6.js');

foam.CLASS({
  name: 'ApiMatrix',
  package: 'org.chromium.apis.web',
  implements: ['foam.mlang.Expressions'],

  documentation: `ApiMatrix is a client side object that has methods
    to retrieve releaseDAO, webInterfaceDAO and their junction DAO, transform
    to CSV format string or JSON matrix that can be displayed on HTML as
    a nested table.`,

  requires: [
    'foam.dao.AnonymousSink',
    'foam.dao.ArraySink',
    'org.chromium.apis.web.Release',
    'org.chromium.apis.web.ReleaseWebInterfaceJunction',
    'org.chromium.apis.web.WebInterface',
  ],
  imports: ['releaseDAO', 'releaseWebInterfaceJunctionDAO', 'webInterfaceDAO'],

  properties: [
    {
      class: 'Boolean',
      name: 'runInParallel',
      documentation: 'Support computing multiple matrices at once.',
    },
    {
      class: 'Int',
      name: 'matrixId_',
      documentation: `Store an ID of the latest matrix being computed. This
          allows old computations to be aborted.`,
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
          documentation: `Valid release keys used to filter
              releaseWebInterfaceJunctionDAO.`,
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
        const id = this.runInParallel ? 0 : ++this.matrixId_;
        return this.verifyReleaseKeys_(releaseKeys).then(() => {
          if (id && this.matrixId_ !== id) return null;

          options = options || {};
          let matrix = {};

          let query = this.IN(this.ReleaseWebInterfaceJunction.SOURCE_ID,
                              releaseKeys);
          if (options.searchKey) {
            query = this.AND(
                query,
                this.CONTAINS_IC(this.ReleaseWebInterfaceJunction.TARGET_ID,
                                 options.searchKey));
          }

          return new Promise((resolve, reject) => {
            // TODO(markdittmer): Second argument in sink.put() should be a
            // subscription, so "detached" here should be unnecessary. Clean
            // this up once https://github.com/foam-framework/foam2/issues/633
            // is fixed.
            let detached = false;

            let promises = [];
            return this.releaseWebInterfaceJunctionDAO.where(query)
                .select(this.AnonymousSink.create({
                  sink: {
                    put: junction => {
                      if (detached) return;
                      if (id && this.matrixId_ !== id) {
                        detached = true;
                        resolve(null);
                        return;
                      }

                      promises.push(
                          this.webInterfaceDAO.find(junction.targetId)
                            .then(iface => {
                              if (detached) return;
                              if (id && this.matrixId_ !== id) {
                                detached = true;
                                resolve(null);
                                return;
                              }

                              const k0 = iface.interfaceName;
                              const k1 = iface.apiName;
                              const k2 = junction.sourceId;
                              // Function and Object's built-in methods can be
                              // interface name or API name.  So matrix[k0] is
                              // not sufficient to check if k0 exists in matrix,
                              // use hasOwnProperty instead.
                              if (!matrix.hasOwnProperty(k0)) {
                                matrix[k0] = {};
                              }
                              // "hasOwnProperty" could also be a apiName,
                              // thus use Object.prototype.hasOwnProperty
                              // to ensure we invoke the right function.
                              if (!Object.prototype.hasOwnProperty.call(
                                  matrix[k0], k1)) {
                                matrix[k0][k1] = {};
                              }
                              matrix[k0][k1][k2] = true;
                            }));
                    },
                  },
                })).then(() => {
                  if (detached) return null;
                  if (id && this.matrixId_ !== id) {
                    resolve(null);
                    return null;
                  }

                  return Promise.all(promises);
                }).then(results => {
                  if (results === null) return null;
                  if (id && this.matrixId_ !== id) {
                    resolve(null);
                    return null;
                  }

                  return this.filterMatrix_(matrix, options);
                }).then(matrix => {
                  if (matrix === null) return null;
                  if (id && this.matrixId_ !== id) {
                    resolve(null);
                    return null;
                  }

                  resolve(matrix);
                  return matrix;
                });
          });
        });
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
        return this.releaseWebInterfaceJunctionDAO
          .where(this.IN(
            this.ReleaseWebInterfaceJunction.SOURCE_ID, releaseKeys))
          .select(this.GROUP_BY(
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
    {
      name: 'verifyReleaseKeys_',
      documentation: 'Confirm that input releaseKeys exist in releaseDAO.',
      args: [
        {
          name: 'releaseKeys',
          typeName: 'Array',
          documentation: 'Primary keys expected to appear in releaseDAO.',
        }
      ],
      returns: {
        typeName: 'Promise',
        documentation: `A promise that resolves iff all input releaseKeys exist
            in releaseDAO, otherwise rejects.`
      },

      code: function(releaseKeys) {
        return Promise.all(releaseKeys.map(
            (releaseId) => this.releaseDAO.find(releaseId).then((release) => {
              if (release === null)
                throw new Error(`${releaseId} does not exist.`);
            })));
      },
    },
  ],
});

foam.CLASS({
  name: 'ApiMatrixController',
  package: 'org.chromium.apis.web',
  implements: ['foam.mlang.Expressions'],

  requires: [
    'org.chromium.apis.web.ApiMatrix',
    'org.chromium.apis.web.Release',
  ],
  imports: ['releaseDAO', 'releaseWebInterfaceJunctionDAO'],

  topics: ['newMatrix'],

  properties: [
    {
      class: 'FObjectArray',
      of: 'org.chromium.apis.web.Release',
      name: 'releases',
    },
    {
      class: 'String',
      name: 'searchKey',
    },
    {
      name: 'releaseOptions',
      value: null,
    },
    {
      name: 'numAvailable',
      value: null,
    },
    {
      class: 'FObjectArray',
      of: 'org.chromium.apis.web.ReleaseWebInterfaceJunction',
      name: 'data_',
    },
    {
      class: 'FObjectProperty',
      of: 'org.chromium.apis.web.ApiMatrix',
      name: 'apiMatrix_',
      factory: function() { return this.ApiMatrix.create(); },
    },
    {
      name: 'matrix_',
      factory: function() { return {}; },
    },
    {
      name: 'matrixPromise_',
      value: Promise.resolve({}),
    },
  ],

  methods: [
    {
      name: 'getReleaseGroups',
      returns: 'Promise',
      code: function() {
        return this.apiMatrix_.getReleases();
      },
    },
    {
      name: 'getLatestReleases',
      returns: 'Promise',
      code: function() {
        return this.releaseDAO.orderBy(this.DESC(this.Release.RELEASE_DATE))
            .select().then(sink => {
              const array = sink.array;

              let latestReleaseMap = {};
              let latestReleases = [];
              let releases = Array.from(array);

              for (let i = 0; i < releases.length; i++) {
                const release = releases[i];
                if (!latestReleaseMap[release.browserName] &&
                    (release.osName === 'Windows' ||
                     release.browserName === 'Safari')) {
                  latestReleases.push(release);
                  latestReleaseMap[release.browserName] = true;
                }
              }

              return latestReleases;
            });
      },
    },
    {
      name: 'setLatestReleases',
      returns: 'Promise',
      code: function() {
        return this.getLatestReleases().then(latest => {
          this.setReleases(latest);
          return latest;
        });
      },
    },
    {
      name: 'getMatrixCSV',
      returns: 'Promise',
      code: function(releaseKeys) {
        return this.matrixPromise_
            .then(matrix => this.apiMatrix_.matrixToCSV(releaseKeys, matrix));
      },
    },
    function setReleases(releases) {
      console.log('ApiMatrixController.setReleases', releases);
      this.releases = releases;
    },
    function setOpts(opts) {
      console.log('ApiMatrixController.setOpts', opts);
      this.searchKey = opts.searchKey || '';
      this.releaseOptions = opts.releaseOptions || null;
      this.numAvailable = opts.numAvailable || null;
    },

    function init() {
      this.releases$.sub(this.onChanges);
      this.searchKey$.sub(this.onChanges);
      this.releaseOptions$.sub(this.onChanges);
      this.numAvailable$.sub(this.onChanges);

      this.releaseWebInterfaceJunctionDAO.on.reset.sub(this.onChanges);
    },
  ],

  listeners: [
    {
      name: 'onChanges',
      isMerged: true,
      mergeDelay: 10,
      code: function() {
        console.log('ApiMatrixController.onChanges',
                    'releases', this.releases,
                    'searchKey', this.searchKey,
                    'releaseOptions', this.releaseOptions,
                    'numAvailable', this.numAvailable);
        const releaseKeys = this.releases.map(release => release.releaseKey);
        const DEBUG_prevMatrix = this.apiMatrix_.matrixId_;
        this.matrixPromise_ = this.apiMatrix_.toMatrix(releaseKeys, {
          searchKey: this.searchKey,
          releaseOptions: this.releaseOptions,
          numAvailable: this.numAvailable,
        }).then(matrix => {
          console.log('ApiMatrixController.onChanges',
                      'prev ID', DEBUG_prevMatrix,
                      'matrix', matrix);
          // TODO(markdittmer): This should be more clever. Only update when
          // !equals(matrix, this.matrix_).
          matrix && this.newMatrix.pub(matrix);
          this.matrix_ = matrix || this.matrix_;
          return this.matrix_;
        });
      },
    },
  ],
});
