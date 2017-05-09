// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

foam.CLASS({
  name: 'FailureToShipData',
  package: 'org.chromium.apis.web',
  documentation: `Number of APIs that have been shipping in all other browsers
      for a one-year grace period, but have never been shipped in this
      browser.`,
  ids: ['browserName', 'date'],
  properties: [
    {
      class: 'String',
      name: 'browserName',
      documentation: 'Name of the browser associated with this metric.',
      required: true,
      final: true,
    },
    {
      class: 'FObjectProperty',
      of: 'org.chromium.apis.web.Release',
      name: 'release',
      documentation: `The latest release of browser named "browserName" as of
          "date".`,
      required: true,
      final: true,
    },
    {
      class: 'Date',
      name: 'date',
      documentation: 'The date for which this metric is computed.',
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
      class: 'Array',
      name: 'prevReleases',
      documentation: `Releases named "browserName" prior to "release".`,
      required: true,
      final: true,
    },
    {
      class: 'Array',
      name: 'comparedReleases',
      documentation: `Releases not named "browserName" that were released
          during the one-year grace period.`,
      required: true,
      final: true,
    },
  ],
});
