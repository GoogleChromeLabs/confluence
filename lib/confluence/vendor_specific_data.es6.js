// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

foam.CLASS({
  name: 'VendorSpecificData',
  package: 'org.chromium.apis.web',
  documentation: `This class contains a browser and its
    number of failure to ship APIs.`,
  ids: ['browserName', 'date'],
  properties: [
    {
      name: 'browserName',
      class: 'String',
      documentation: 'Name of the Browser',
      required: true,
      final: true,
    },
    {
      name: 'browser',
      documentation: `An Browser object used to calculate failure to ship
        metric with other verndors at this.date.`,
      required: true,
      final: true,
    },
    {
      name: 'browserVersion',
      class: 'String',
      documentation: 'Version of this browser.',
      required: true,
      final: true,
    },
    {
      name: 'date',
      class: 'Date',
      documentation: 'The date at which this metric is computed.',
      required: true,
      final: true,
    },
    {
      name: 'numVendorSpecific',
      class: 'Int',
      documentation: 'The number of verndor-specific APIs.',
      required: true,
      final: true,
    },
    {
      name: 'prevReleaseBrowsers',
      documentation: `An array of Browser objects that is previous releases
          of this browser.`,
      required: true,
      final: true,
    },
    {
      name: 'comparedBrowsers',
      documentation: `An array of browser objects used to be compared with
          this browser.`,
      required: true,
      final: true,
    },
  ],
});
