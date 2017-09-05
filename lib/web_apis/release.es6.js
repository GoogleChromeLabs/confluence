// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

foam.CLASS({
  name: 'Release',
  package: 'org.chromium.apis.web',
  ids: ['releaseKey'],
  properties: [
    {
      class: 'String',
      name: 'browserName',
      documentation: `The name of release.`,
      required: true,
      final: true,
    },
    {
      class: 'String',
      name: 'browserVersion',
      documentation: `The version of release.`,
      required: true,
      final: true,
    },
    {
      class: 'String',
      name: 'osName',
      documentation: `The name of operating system as reported by the
        object-graph-js library.`,
      required: true,
      final: true,
    },
    {
      class: 'String',
      name: 'osVersion',
      documentation: `The version of operating system as reported by the
        object-graph-js library.`,
      required: true,
      final: true,
    },
    {
      class: 'String',
      name: 'releaseKey',
      documentation: `An unique key for this release. Avoid the need for
        CONCAT mLang or similar to be able to groupBy browserName,
        browserVersion, osName, osVersion.`,
      expression: function(browserName, browserVersion, osName, osVersion) {
        return`${browserName}_${browserVersion}_${osName}_${osVersion}`;
      },
    },
    {
      class: 'Date',
      name: 'releaseDate',
      documentation: `The date when this version is released.`,
      required: true,
      final: true,
    },
  ],
});

// Initialized Versioned form of class for SyncDAOs.
foam.version.VersionedClassFactorySingleton.create()
    .get(org.chromium.apis.web.Release);
