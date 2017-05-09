// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

foam.CLASS({
  name: 'BrowserSpecificData',
  package: 'org.chromium.apis.web',
  documentation: `Count of APIs that:

      (1) Have been shipping in this browser for the duration of a one-year
          grace period,
      and
      (2) Have not shipped in any other browser at any point during the
          one-year grace period.`,
  ids: ['browserName', 'date'],
  properties: [
    {
      class: 'String',
      name: 'browserName',
      documentation: 'Name of browser for this metric.',
      required: true,
      final: true,
    },
    {
      class: 'FObjectProperty',
      of: 'org.chromium.apis.web.Release',
      name: 'release',
      documentation: `The latest release of this browser as of "date".`,
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
      name: 'numBrowserSpecific',
      documentation: 'The number of browser-specific APIs.',
      required: true,
      final: true,
    },
    {
      class: 'Array',
      name: 'prevReleases',
      documentation: `Releases of this browser released prior to "release".`,
      required: true,
      final: true,
    },
    {
      class: 'Array',
      name: 'comparedReleases',
      documentation: `Releases of other browsers (not "release"'s browser)
          released during the one-year grace period prior to "date".`,
      required: true,
      final: true,
    },
  ],
});
