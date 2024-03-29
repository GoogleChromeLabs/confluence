// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

foam.CLASS({
  name: 'ApiCountData',
  package: 'org.chromium.apis.web',
  documentation: `API counts that Nth release to N-1th release of a browser.
      Counts are: {<total APIs>, <removed APIs>, <added APIs>}.`,
  ids: ['browserName', 'releaseDate'],
  properties: [
    {
      class: 'Date',
      name: 'releaseDate',
      documentation: `The official release date for this release.`,
      required: true,
      final: true,
    },
    {
      class: 'String',
      name: 'browserName',
      documentation: `Name of the browser associated with this metric.`,
      required: true,
      final: true,
    },
    {
      class: 'FObjectProperty',
      of: 'org.chromium.apis.web.Release',
      name: 'currRelease',
      documentation: `The release from "releaseDate".`,
      value: null,
      final: true,
    },
    {
      class: 'FObjectProperty',
      of: 'org.chromium.apis.web.Release',
      name: 'prevRelease',
      documentation: `The release prior to "currRelease" of the same browser.`,
      value: null,
      final: true,
    },
    {
      class: 'Int',
      name: 'totalApis',
      documentation: `The number of total APIs in "currRelease".`,
      required: true,
      final: true,
    },
    {
      class: 'Int',
      name: 'newApis',
      documentation: `The number of new APIs introduced in "currRelease".`,
      required: true,
      final: true,
    },
    {
      class: 'Int',
      name: 'removedApis',
      documentation: `The number of removed APIs removed in "currRelease".`,
      required: true,
      final: true,
    },
  ],
});
