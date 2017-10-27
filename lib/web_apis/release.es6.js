// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

foam.CLASS({
  name: 'Release',
  package: 'org.chromium.apis.web',
  ids: ['releaseKey'],

  constants: {
    // HACK(markdittmer): A bunch of Safari data were collected before userAgent
    // scraping fetched the Safari version number (rather than the WebKit
    // version number). This constant provides version number translation.
    FRIENDLY_BROWSER_VERSIONS: {
      Safari: {
        '534': '5.1',
        '536': '6.0',
        '537.43': '6.1',
        '537.71': '7.0',
        '537.73': '7.0',
        '537.75': '7.0',
        '537.76': '7.0',
        '537.77': '7.0',
        '537.78': '7.0',
        '537.85': '7.1',
        '538': '8.0',
        '600': '8.0',
        '601.1': '9.0',
        '601.2': '9.0',
        '601.3': '9.0',
        '601.4': '9.0',
        '601.5': '9.1',
        '601.6': '9.1',
        '601.7': '9.1',
        '602': '10.0',
        '603': '10.1',
      },
    },
  },

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
      name: 'friendlyBrowserVersion',
      transient: true,
      getter: function() {
        const names = this.FRIENDLY_BROWSER_VERSIONS;
        if (!names[this.browserName]) return this.browserVersion;
        const versionNames = names[this.browserName];
        const version = this.browserVersion.split('.');
        for (let i = 1; i <= version.length; i++) {
          const versionStr = version.slice(0, i).join('.');
          if (versionNames[versionStr]) return versionNames[versionStr];
        }
        return this.browserVersion;
      }
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
      // Provide default releaseDate; default object should not have undefined.
      factory: function() { return new Date(0); },
    },
    {
      class: 'Boolean',
      name: 'isMobile',
      documentation: 'Is this a mobile release?',
      expression: function(osName) {
        return osName === 'Android' || osName === 'iPhone';
      },
      transient: true,
    },
  ],
});
