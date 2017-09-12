// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

// TODO(markdittmer): This should probably live in a DAO of objects, rather than
// a custom map-like interface.
foam.CLASS({
  name: 'VersionHistory',
  package: 'org.chromium.apis.web',
  documentation: `Official release dates for each browser release.`,

  properties: [
    {
      name: 'releaseHistory',
      documentation: `JSON of the form. :
          {<browserName>: {<browserVersion>: <releaseDate>, ...} ...}
          The release dates for releases are collected manually and stored in
          JSON file.`,
      required: true,
      final: true,
    },
  ],
  methods: [
    {
      name: 'getReleaseDate',
      documentation: `Find the release date for a given browser release.`,
      args: [
        {
          name: 'releasekey',
          typeName: 'String',
          documentation: 'A string of valid release key',
        },
      ],
      returns: {
        typeName: 'Date',
        documentation: 'The release date of the given browser release.',
      },
      code: function(browserName, browserVersion) {
        if (!this.releaseHistory.hasOwnProperty(browserName)) {
          throw new Error(`${browserName} not found in release history.`);
        }
        let versionHistory = this.releaseHistory[browserName];
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
