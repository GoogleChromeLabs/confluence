// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

foam.CLASS({
  name: 'FailureToShipData',
  package: 'org.chromium.apis.web',
  documentation: `This class contains a browser and its
    number of failure to ship APIs.`,
  ids: ['browserName', 'date'],
  properties: [
    {
      class: 'String',
      name: 'browserName',
      documentation: 'Name of the Browser',
      required: true,
      final: true,
    },
    {
      class: 'FObjectProperty',
      of: 'org.chromium.apis.web.Browser',
      name: 'browser',
      documentation: `An Browser object used to calculate failure to ship
        metric with other verndors at this.date.`,
      required: true,
      final: true,
    },
    {
      class: 'Date',
      name: 'date',
      documentation: 'The date at which this metric is computed.',
      required: true,
      final: true,
    },
    {
      class: 'Int',
      name: 'numFailureToShip',
      documentation: 'The number of failure-to-ship APIs.',
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
