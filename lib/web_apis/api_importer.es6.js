// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

require('./release.es6.js');
require('./web_interface.es6.js');
require('./release_interface_relationship.es6.js');
require('./version_history.es6.js');

foam.CLASS({
  name: 'ApiImporter',
  package: 'org.chromium.apis.web',

  documentation: `Imports ApiExtractor.webCatalog into Release, WebInterface,
      and ReleaseWebInterfaceJunction DAOs.`,

  requires: [
    'org.chromium.apis.web.Release',
    'org.chromium.apis.web.WebInterface',
    'org.chromium.apis.web.ReleaseWebInterfaceJunction',
    'org.chromium.apis.web.VersionHistory',
    'foam.mlang.ExpressionsSingleton',
    'foam.dao.EasyDAO',
  ],
  imports: ['releaseDAO', 'webInterfaceDAO'],

  properties: [
    {
      name: 'versionHistory',
      documentation: `An helper gets release date for a browser version.`,
      factory: function() {
        return this.VersionHistory.create({
          releaseHistory: require('../../data/version_history.json'),
        });
      },
    },
  ],

  methods: [
    {
      name: 'import',
      documentation: `An asynchronous function to import interface/API from
        web catalog for a given release.`,
      args: [
        {
          name: 'browserName',
          documentation: `The name of imported browser.`,
          typeName: 'String',
        },
        {
          name: 'browserVersion',
          documentation: `The version of imported browser.`,
          typeName: 'String',
        },
        {
          name: 'osName',
          documentation: `The name of operating system.`,
          typeName: 'String',
        },
        {
          name: 'osVersion',
          documentation: `The version of operating system.`,
          typeName: 'String',
        },
        {
          name: 'apiCatalog',
          documentation: `The web catalog of this release, tested on the given
              operating system and version. The json object should be the JSON
              object returned from com.web.catalog.ApiExtractor.`,
          typeName: 'JSON',
        },
      ],
      code: function(browserName, browserVersion,
                     osName, osVersion, apiCatalog) {
        let promises = [];
        let release = this.Release.create({
          browserName,
          browserVersion,
          osName,
          osVersion,
          releaseDate: this.versionHistory
              .getReleaseDate(browserName, browserVersion),
        });
        promises.push(this.releaseDAO.put(release));
        let interfaceNames = Object.keys(apiCatalog);
        for (let i = 0; i < interfaceNames.length; i++) {
          let interfaceName = interfaceNames[i];
          for (let j = 0; j < apiCatalog[interfaceName].length; j++) {
            let apiName = apiCatalog[interfaceName][j];
            let webInterface = this.WebInterface.create({
              interfaceName,
              apiName,
            });
            promises.push(this.webInterfaceDAO.put(webInterface));
            promises.push(webInterface.releases.add(release));
          }
        }
        return Promise.all(promises);
      },
    },
  ],
});
