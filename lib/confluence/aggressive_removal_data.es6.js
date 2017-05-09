// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

foam.CLASS({
  name: 'AggressiveRemovalData',
  package: 'org.chromium.apis.web',
  documentation: `Aggressive removal count and related browsers metadata.`,
  ids: ['browserName', 'date'],
  properties: [
    {
      class: 'String',
      name: 'browserName',
      documentation: 'Browser name associated with count.',
      required: true,
      final: true,
    },
    {
      class: 'FObjectProperty',
      of: 'org.chromium.apis.web.Release',
      name: 'releaseOneYearAgo',
      documentation: `A release object that is released just before
          currentDate - 1-year grace period.`,
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
      name: 'numAggressiveRemoval',
      documentation: 'The number of aggressive-removal APIs.',
      required: true,
      final: true,
    },
    {
      class: 'Array',
      name: 'prevReleases',
      documentation: `Releases from "browserName" (also from the same platform)
          that were released prior to "releaseOneYearAgo".`,
      required: true,
      final: true,
    },
    {
      class: 'Array',
      name: 'currReleases',
      documentation: `Current releases, as of "date", from other browsers
          (r.browserName !== this.browserName).`,
      required: true,
      final: true,
    },
  ],
});
