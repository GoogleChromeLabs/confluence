// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

foam.CLASS({
  name: 'VersionHistory',
  package: 'org.chromium.apis.web',
  documentation: `VersionHistory is a class that stores official release
    dates for each browsers and versions. It has methods to obtain
    release date for a given browser and version.`,
  properties: [
    {
      name: 'browserHistory',
      documentation: `JSON of the form. :
        {browserName: {versionNumber: releaseDate ...} ...}
        The release dates for major browsers are collected manually
        and stored in JSON file.`,
      required: true,
      final: true,
    },
  ],
  methods: [
    {
      name: 'getReleaseDate',
      documentation: `Find the release date for a given browser and version.`,
      args: [
        {
          name: 'browserkey',
          typeName: 'String',
          documentation: 'A string of valid browser key',
        },
      ],
      returns: {
        typeName: 'Date',
        documentation: 'The release date of the given browser version.',
      },
      code: function(browserName, browserVersion) {
        if (!this.browserHistory.hasOwnProperty(browserName)) {
          throw new Error(`${browserName} not found in browser history.`);
        }
        let versionHistory = this.browserHistory[browserName];
        // In version_history.json, we only stores major version for
        // Firefox and Chrome. So use indexOf to see if the given version
        // matches the major version.
        for (let version in versionHistory) {
          if (!versionHistory.hasOwnProperty(version)) continue;
          if (browserVersion.indexOf(version) === 0) {
            return new Date(versionHistory[version]);
          }
        }
        throw new Error(`Version ${browserVersion} not ` +
          `found in ${browserName} history.`);
      },
    },
  ],
});
