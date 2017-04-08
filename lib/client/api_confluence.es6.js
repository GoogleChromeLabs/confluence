// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

require('../confluence/api_velocity_data.es6.js');
require('../confluence/failure_to_ship_data.es6.js');
require('../confluence/vendor_specific_data.es6.js');

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
  ],
  properties: [
    {
      name: 'apiVelocityDAO',
      documentation: `A DAO that contains ApiVelocity objects.`,
      typeName: 'ApiVelocity DAO',
      required: true,
      final: true,
    },
    {
      name: 'failureToShipDAO',
      documentation: `A DAO that contains number of failur-to-ship APIs for
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
    // {
    //   name: 'aggressiveRemovalDAO',
    //   documentation: `A DAO that contains number of aggressive removal APIs for
    //     each browser associated with date.`,
    //   typeName: 'BrowserDataPoint DAO',
    //   required: true,
    //   final: true,
    // },
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
      name: 'getApiVelocity',
      documentation: `Read from apiVelocityDAO and returns API velocity
        metric for each brwoser.`,
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
                browserKey,
                prevBrowserKey: previous versions's browserKey.,
                totalApis: total number of inteface APIs in this browser.,
                newApis: number of new APIs compared to previous version.,
                removedApis: number of removed APIs.,
              }, ...
            ], ...}`,
      },
      code: function() {
        return this.apiVelocityDAO.select(this.mlang.GROUP_BY(
          this.ApiVelocityData.BROWSER_NAME,
          this.ArraySink.create())).then((groups) => {
            let apiVelocityResult = {};
            for (let i = 0; i < groups.groupKeys.length; i++) {
              let browserName = groups.groupKeys[i];
              apiVelocityResult[browserName] =
                groups.groups[browserName].a.map((apiVelocityData) => {
                  return {
                    releaseDate: new Date(apiVelocityData.releaseDate),
                    browserName: apiVelocityData.browserName,
                    browserVersion: apiVelocityData.currBrowser.browserVersion,
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
      documentation: `Read from failure and returns API velocity
        metric for each brwoser.`,
      returns: {
        typeName: 'JSON',
        documentation: `JSON of the form:
            {browserName: [
              {browserKey, totalAPI, newAPI, removedAPI}...
            ], ...}`,
      },
      code: function() {
        return this.failureToShipDAO
          .orderBy(this.FailureToShipData.DATE)
          .select(this.mlang.GROUP_BY(
            this.FailureToShipData.BROWSER_NAME,
            this.ArraySink.create())).then((groups) => {
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
      documentation: `Read from vendor specific API dao and returns
        vendor specific metric for each brwoser.`,
      returns: {
        typeName: 'JSON',
        documentation: `JSON of the form:
            {browserName: {osName: [
              {browserKey, totalAPI, newAPI, removedAPI}...
            ] ...} ... } ...}`,
      },
      code: function() {
        return this.vendorSpecificDAO
          .orderBy(this.VendorSpecificData.DATE)
          .select(this.mlang.GROUP_BY(
            this.FailureToShipData.BROWSER_NAME,
            this.ArraySink.create())).then((groups) => {
              let vendorSpecificResult = {};
              groups.groupKeys.forEach((bName) => {
                vendorSpecificResult[bName] = groups.groups[bName].a;
              });
              return vendorSpecificResult;
            });
      },
    },
    // {
    //   name: 'getAggressiveRemoval',
    //   documentation: `Read from failure and returns API velocity
    //     metric for each brwoser.`,
    //   returns: {
    //     typeName: 'JSON',
    //     documentation: `JSON of the form:
    //         {browserName: {osName: [
    //           {browserKey, totalAPI, newAPI, removedAPI}...
    //         ] ...} ... } ...}`,
    //   },
    //   code: function() {
    //     return this.aggressiveRemovalDAO
    //       .orderBy(this.RemovedAPI.BROWSER_KEY)
    //       .select(this.mlang.GROUP_BY(
    //         this.RemovedAPI.BROWSER_NAME_OS_NAME,
    //         this.ArraySink.create()))
    //       .then((groups) => {
    //         let aggressiveRemovalResult = {};
    //         for (let i = 0; i < groups.groupKeys.length; i++) {
    //           let browserOsKey = groups.groupKeys[i];
    //           aggressiveRemovalResult[browserOsKey] =
    //             groups.groups[browserOsKey].a.map((aggressiveRemoval) => {
    //               return {
    //                 releaseDate: new Date(aggressiveRemoval.releaseDate),
    //                 browserName: aggressiveRemoval.browserName,
    //                 browserVersion: aggressiveRemoval.browserVersion,
    //                 osName: aggressiveRemoval.osName,
    //                 aggressiveRemoval: aggressiveRemoval.aggressiveRemoval,
    //                 prevBrowserKey: aggressiveRemoval.prevBrowserKey,
    //                 browserKey: aggressiveRemoval.browserKey,
    //                 comparedBrowserKeys: aggressiveRemoval.comparedBrowserKeys,
    //               };
    //           });
    //         }
    //         return aggressiveRemovalResult;
    //       });
    //   },
    // },
  ],
});
