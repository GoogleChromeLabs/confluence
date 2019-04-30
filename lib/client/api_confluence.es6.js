// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

require('../confluence/api_count_data.es6.js');
require('../confluence/browser_metric_data.es6.js');

foam.CLASS({
  name: 'ApiConfluence',
  package: 'org.chromium.apis.web',
  implements: ['foam.mlang.Expressions'],

  documentation: `ApiConfluence is a client side object that
    has methods to retrieve confluence metrics from Rest DAO such
    as API Count, lone-omission, lone removal and
    browser-specific-APIs.`,

  requires: [
    'foam.dao.ArraySink',
    'org.chromium.apis.web.BrowserMetricData',
    'org.chromium.apis.web.ApiCountData',
  ],

  properties: [
    {
      class: 'FObjectProperty',
      of: 'foam.dao.DAO',
      name: 'apiCountDAO',
      documentation: `A DAO that contains ApiCount objects.`,
      required: true,
      final: true,
    },
    {
      class: 'FObjectProperty',
      of: 'foam.dao.DAO',
      name: 'loneOmissionDAO',
      documentation: `A DAO that contains number of lone-omission APIs for
          each release associated with date.`,
      required: true,
      final: true,
    },
    {
      class: 'FObjectProperty',
      of: 'foam.dao.DAO',
      name: 'browserSpecificDAO',
      documentation: `A DAO that contains number of browser-specific APIs for
          each browser at a particular date.`,
      required: true,
      final: true,
    },
    {
      class: 'FObjectProperty',
      of: 'foam.dao.DAO',
      name: 'loneRemovalDAO',
      documentation: `A DAO that contains number of APIs aggressively removed
          in releases up to a particular date.`,
      required: true,
      final: true,
    },
  ],

  methods: [
    {
      name: 'getApiCount',
      documentation: `Read from apiCountDAO and returns API Count
          metric for each browser release.`,
      returns: {
        typeName: 'Promise',
        documentation: `A Promise for JSON of the form:
            {browserName: [
              [ApiCountData, ...]
            ], ...}`,
      },
      code: function() {
        return this.apiCountDAO
            .select(this.GROUP_BY(
                this.ApiCountData.BROWSER_NAME,
                this.ArraySink.create()))
            .then((groups) => {
              const apiCountResult = {};
              for (let i = 0; i < groups.groupKeys.length; i++) {
                const browserName = groups.groupKeys[i];
                apiCountResult[browserName] =
                  groups.groups[browserName].array.map((apiCountData) => {
                    return {
                      releaseDate: apiCountData.releaseDate.getTime(),
                      browserName: apiCountData.browserName,
                      browserVersion:
                          apiCountData.currRelease.friendlyBrowserVersion,
                      osName: apiCountData.currRelease.osName,
                      osVersion: apiCountData.currRelease.osVersion,
                      totalApis: apiCountData.totalApis,
                      newApis: apiCountData.newApis,
                      removedApis: apiCountData.removedApis,
                      prevRelease: apiCountData.prevRelease,
                      currRelease: apiCountData.currRelease,
                    };
                  });
              }
              console.log('apiCountResult', apiCountResult);
              return apiCountResult;
            });
      },
    },
    {
      name: 'getLoneOmission',
      documentation: `Read from lone omission DAO and return lone omission
          metrics metric for each browser release.`,
      returns: {
        typeName: 'Promise',
        documentation: `A Promise for JSON of the form:
            {browserName: [
              [BrowserMetricData<type=LONE_OMISSION>, ...]
            ], ...}`,
      },
      code: function() {
        return this.loneOmissionDAO
            .orderBy(this.BrowserMetricData.DATE)
            .select(this.GROUP_BY(
                this.BrowserMetricData.BROWSER_NAME,
                this.ArraySink.create()))
            .then((groups) => {
              const loneOmissionResult = {};
              groups.groupKeys.forEach((bName) => {
                loneOmissionResult[bName] = groups.groups[bName].array;
              });
              console.log('loneOmissionResult', loneOmissionResult);
              return loneOmissionResult;
            });
      },
    },
    {
      name: 'getBrowserSpecificApis',
      documentation: `Read from browser-specific API DAO and return
        browser-specific metrics for each browser release.`,
      returns: {
        typeName: 'Promise',
        documentation: `A Promise for JSON of the form:
            {browserName:
              [BrowserMetricData<type=BROWSER_SPECIFIC>, ...]
             ...}`,
      },
      code: function() {
        return this.browserSpecificDAO
            .orderBy(this.BrowserMetricData.DATE)
            .select(this.GROUP_BY(
                this.BrowserMetricData.BROWSER_NAME,
                this.ArraySink.create()))
            .then((groups) => {
              const browserSpecificResult = {};
              groups.groupKeys.forEach((bName) => {
                browserSpecificResult[bName] = groups.groups[bName].array;
              });
              console.log('browserSpecificResult', browserSpecificResult);
              return browserSpecificResult;
            });
      },
    },
    {
      name: 'getLoneRemoval',
      documentation: `Read from lone removal DAO and return lone removal
        metrics for each browser release.`,
      returns: {
        typeName: 'Promise',
        documentation: `A Promise for JSON of the form:
            {browserName:
              [BrowserMetricData<type=LONE_REMOVAL>, ...]
            }`,
      },
      code: function() {
        return this.loneRemovalDAO
            .orderBy(this.BrowserMetricData.DATE)
            .select(this.GROUP_BY(
                this.BrowserMetricData.BROWSER_NAME,
                this.ArraySink.create()))
            .then((groups) => {
              const loneRemovalResult = {};
              groups.groupKeys.forEach((bName) => {
                loneRemovalResult[bName] = groups.groups[bName].array;
              });
              console.log('loneRemovalResult', loneRemovalResult);
              return loneRemovalResult;
            });
      },
    },
  ],
});
