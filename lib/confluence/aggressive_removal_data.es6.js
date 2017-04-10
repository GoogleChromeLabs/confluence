// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

foam.CLASS({
  name: 'AggressiveRemovalData',
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
      name: 'browserOneYearAgo',
      documentation: `An Browser object that is released just before
        date - 1 year time.`,
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
      name: 'numAggressiveRemoval',
      class: 'Int',
      documentation: 'The number of aggressive-removal APIs.',
      required: true,
      final: true,
    },
    {
      name: 'prevReleaseBrowsers',
      documentation: `An array of Browser objects that is previous releases
          of this browser between date - 2 years to date - 3 years.`,
      required: true,
      final: true,
    },
    {
      name: 'currBrowsers',
      documentation: `An array of current versions of other browsers at
        the time of date.`,
      required: true,
      final: true,
    },
  ],
});
