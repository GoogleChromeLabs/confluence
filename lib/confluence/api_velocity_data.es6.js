// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

foam.CLASS({
  name: 'ApiVelocityData',
  package: 'org.chromium.apis.web',
  documentation: `apiVelocityData contains the numer of total APIs,
      new APIs and removed APIs compared to the previous version
      of this browser version.`,
  ids: ['browserName', 'releaseDate'],
  properties: [
    {
      class: 'Date',
      name: 'releaseDate',
      documentation: `The official release date for this browser version.`,
      required: true,
      final: true,
    },
    {
      class: 'String',
      name: 'browserName',
      documentation: `Name of the browser.`,
      required: true,
      final: true,
    },
    {
      class: 'FObjectProperty',
      of: 'org.chromium.apis.web.Browser',
      name: 'currBrowser',
      documentation: `The Browser object of current release.`,
      required: true,
      final: true,
    },
    {
      class: 'FObjectProperty',
      of: 'org.chromium.apis.web.Browser',
      name: 'prevBrowser',
      documentation: `The Browser object of the previous version.`,
      required: true,
      final: true,
    },
    {
      class: 'Int',
      name: 'totalApis',
      documentation: `The number of total Apis in this
          version of browser.`,
      required: true,
      final: true,
    },
    {
      class: 'Int',
      name: 'newApis',
      documentation: `The number of new APIs compared
          to the previous release.`,
      required: true,
      final: true,
    },
    {
      class: 'Int',
      name: 'removedApis',
      documentation: `The number of removed APIs compared
          to the previous release.`,
      required: true,
      final: true,
    },
  ],
});
