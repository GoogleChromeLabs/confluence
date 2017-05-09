// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

require('../confluence/api_velocity_data.es6.js');
require('../confluence/failure_to_ship_data.es6.js');
require('../confluence/browser_specific_data.es6.js');
require('../confluence/aggressive_removal_data.es6.js');

foam.CLASS({
  name: 'ApiConfluence',
  package: 'org.chromium.apis.web',
  documentation: `ApiConfluence is a client side object that
    has methods to retrieve confluence metrics from Rest DAO such
    as API velocity, failure-to-ship, aggressive removal and
    browser-specific-APIs.`,
  requires: [
    'foam.dao.ArraySink',
    'foam.mlang.ExpressionsSingleton',
    'org.chromium.apis.web.ApiVelocityData',
    'org.chromium.apis.web.FailureToShipData',
    'org.chromium.apis.web.BrowserSpecificData',
    'org.chromium.apis.web.AggressiveRemovalData',
  ],
  properties: [
    {
      class: 'FObjectProperty',
      of: 'foam.dao.DAO',
      name: 'apiVelocityDAO',
      documentation: `A DAO that contains ApiVelocity objects.`,
      required: true,
      final: true,
    },
    {
      class: 'FObjectProperty',
      of: 'foam.dao.DAO',
      name: 'failureToShipDAO',
      documentation: `A DAO that contains number of failure-to-ship APIs for
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
      name: 'aggressiveRemovalDAO',
      documentation: `A DAO that contains number of APIs aggressively removed
          in releases up to a particular date.`,
      required: true,
      final: true,
    },
    {
      name: 'mlang',
      documentation: 'The mlang expressions singleton.',
      factory: function() {
        return this.ExpressionsSingleton.create();
      },
    },
  ],
  methods: [
    {
      name: 'getApiVelocity',
      documentation: `Read from apiVelocityDAO and returns API velocity
          metric for each browser release.`,
      returns: {
        typeName: 'JSON',
        documentation: `JSON of the form:
            {browserName: [
              {
                releaseDate,
                browserName,
                browserVersion,
                osName,
                osVersion,
                currRelease: The release object which is released at
                    "releaseDate",
                prevRelease: <release object of release just before
                    "currRelease">,
                totalApis: <total number of APIs in "currRelease">,
                newApis: <number of new APIs in "currRelease">,
                removedApis: <number of removed APIs in "currRelease">,
              }, ...
            ], ...}`,
      },
      code: function() {
        return this.apiVelocityDAO
            .select(this.mlang.GROUP_BY(
                this.ApiVelocityData.BROWSER_NAME,
                this.ArraySink.create()))
            .then((groups) => {
              let apiVelocityResult = {};
              for (let i = 0; i < groups.groupKeys.length; i++) {
                let browserName = groups.groupKeys[i];
                apiVelocityResult[browserName] =
                  groups.groups[browserName].a.map((apiVelocityData) => {
                    return {
                      releaseDate: apiVelocityData.releaseDate,
                      browserName: apiVelocityData.browserName,
                      browserVersion:
                          apiVelocityData.currRelease.browserVersion,
                      osName: apiVelocityData.currRelease.osName,
                      osVersion: apiVelocityData.currRelease.osVersion,
                      totalApis: apiVelocityData.totalApis,
                      newApis: apiVelocityData.newApis,
                      removedApis: apiVelocityData.removedApis,
                      prevRelease: apiVelocityData.prevRelease,
                      currRelease: apiVelocityData.currRelease,
                    };
                });
              }
              return apiVelocityResult;
            });
      },
    },
    {
      name: 'getFailureToShip',
      documentation: `Read from failure to ship DAO and return failure
          to ship metrics metric for each browser release.`,
      returns: {
        typeName: 'JSON',
        documentation: `JSON of the form:
            {browserName: [
              [FailureToShipData, ...]
            ], ...}`,
      },
      code: function() {
        return this.failureToShipDAO
            .orderBy(this.FailureToShipData.DATE)
            .select(this.mlang.GROUP_BY(
                this.FailureToShipData.BROWSER_NAME,
                this.ArraySink.create()))
            .then((groups) => {
              let failureToShipResult = {};
              groups.groupKeys.forEach((bName) => {
                failureToShipResult[bName] = groups.groups[bName].a;
              });
              return failureToShipResult;
            });
      },
    },
    {
      name: 'getBrowserSpecificApis',
      documentation: `Read from browser-specific API DAO and return
        browser-specific metrics for each browser release.`,
      returns: {
        typeName: 'JSON',
        documentation: `JSON of the form:
            {browserName:
              [BrowserSpecificData, ...]
             ...}`,
      },
      code: function() {
        return this.browserSpecificDAO
            .orderBy(this.BrowserSpecificData.DATE)
            .select(this.mlang.GROUP_BY(
                this.BrowserSpecificData.BROWSER_NAME,
                this.ArraySink.create()))
            .then((groups) => {
              let browserSpecificResult = {};
              groups.groupKeys.forEach((bName) => {
                browserSpecificResult[bName] = groups.groups[bName].a;
              });
              return browserSpecificResult;
            });
      },
    },
    {
      name: 'getAggressiveRemoval',
      documentation: `Read from aggressive removal DAO and return aggressive
        removal metrics for each browser release.`,
      returns: {
        typeName: 'JSON',
        documentation: `JSON of the form:
            {browserName:
              [AggressiveRemovalData, ...]
            }`,
      },
      code: function() {
        return this.aggressiveRemovalDAO
            .orderBy(this.AggressiveRemovalData.DATE)
            .select(this.mlang.GROUP_BY(
                this.AggressiveRemovalData.BROWSER_NAME,
                this.ArraySink.create()))
            .then((groups) => {
              let aggressiveRemovalResult = {};
              groups.groupKeys.forEach((bName) => {
                aggressiveRemovalResult[bName] = groups.groups[bName].a;
              });
              return aggressiveRemovalResult;
            });
      },
    },
  ],
});
