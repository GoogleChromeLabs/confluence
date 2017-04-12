// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

require('../confluence/api_velocity_data.es6.js');
require('../confluence/failure_to_ship_data.es6.js');
require('../confluence/vendor_specific_data.es6.js');
require('../confluence/aggressive_removal_data.es6.js');

foam.CLASS({
  name: 'ApiConfluence',
  package: 'org.chromium.apis.web',
  documentation: `ApiConfluence is a client side object that
    has methods to retrieve confluence metrics from Rest DAO such
    as API velocity, failure-to-ship, aggressive removal and
    vendor-specific-APIs.`,
  requires: [
    'foam.dao.ArraySink',
    'foam.mlang.ExpressionsSingleton',
    'org.chromium.apis.web.ApiVelocityData',
    'org.chromium.apis.web.FailureToShipData',
    'org.chromium.apis.web.VendorSpecificData',
    'org.chromium.apis.web.AggressiveRemovalData',
  ],
  properties: [
    {
      name: 'apiVelocityDAO',
      documentation: `A DAO that contains ApiVelocity objects.`,
      typeName: 'ApiVelocityDataDAO',
      required: true,
      final: true,
    },
    {
      name: 'failureToShipDAO',
      documentation: `A DAO that contains number of failure-to-ship APIs for
          each browser associated with date.`,
      typeName: 'failureToShipDataDAO',
      required: true,
      final: true,
    },
    {
      name: 'vendorSpecificDAO',
      documentation: `A DAO that contains number of vendor-specific APIs for
          each browser associated with date.`,
      typeName: 'vendorSpecificDataDAO',
      required: true,
      final: true,
    },
    {
      name: 'aggressiveRemovalDAO',
      documentation: `A DAO that contains number of aggressive removal APIs for
          each browser associated with date.`,
      typeName: 'aggressiveRemovalDataDAO',
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
          metric for each browser.`,
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
                currBrowser: The browser object which is released at
                    the releaseDate,
                prevBrowser: <browser object of previous release>,
                totalApis: <total number of APIs>,
                newApis: <number of new APIs>,
                removedApis: <number of removed APIs>,
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
                          apiVelocityData.currBrowser.browserVersion,
                      osName: apiVelocityData.currBrowser.osName,
                      osVersion: apiVelocityData.currBrowser.osVersion,
                      totalApis: apiVelocityData.totalApis,
                      newApis: apiVelocityData.newApis,
                      removedApis: apiVelocityData.removedApis,
                      prevBrowser: apiVelocityData.prevBrowser,
                      currBrowser: apiVelocityData.currBrowser,
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
          to ship metrics metric for each browser.`,
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
      name: 'getVendorSpecificApis',
      documentation: `Read from vendor-specific API DAO and return
        vendor-specific metrics for each browser.`,
      returns: {
        typeName: 'JSON',
        documentation: `JSON of the form:
            {browserName:
              [VendorSpecificData, ...]
             ...}`,
      },
      code: function() {
        return this.vendorSpecificDAO
            .orderBy(this.VendorSpecificData.DATE)
            .select(this.mlang.GROUP_BY(
                this.VendorSpecificData.BROWSER_NAME,
                this.ArraySink.create()))
            .then((groups) => {
              let vendorSpecificResult = {};
              groups.groupKeys.forEach((bName) => {
                vendorSpecificResult[bName] = groups.groups[bName].a;
              });
              return vendorSpecificResult;
            });
      },
    },
    {
      name: 'getAggressiveRemoval',
      documentation: `Read from aggressive removal DAO and return aggressive
        removal metrics for each browser.`,
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
