// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

require('./browser.es6.js');
require('./web_interface.es6.js');
require('./browser_interface_relationship.es6.js');
require('./version_history.es6.js');

foam.CLASS({
  name: 'ApiImporter',
  package: 'org.chromium.apis.web',

  documentation: `API Importer is a class that handles importing browserAPIs
    from webCatalog object to browserAPI DAO.`,
  requires: [
    'org.chromium.apis.web.Browser',
    'org.chromium.apis.web.WebInterface',
    'org.chromium.apis.web.BrowserWebInterfaceJunction',
    'org.chromium.apis.web.VersionHistory',
    'foam.mlang.ExpressionsSingleton',
    'foam.dao.EasyDAO',
  ],
  exports: [
    'browserDAO',
    'interfaceDAO as webInterfaceDAO',
    'browserApiDAO as browserWebInterfaceJunctionDAO',
  ],
  properties: [
    {
      name: 'browserApiDAO',
      documentation: `A DAO that contains pairs of browser
        and web interface.`,
      factory: function() {
        return this.EasyDAO.create({
          name: 'browserWebInterfaceJunctionDAO',
          of: this.BrowserWebInterfaceJunction,
          daoType: 'MDAO',
        });
      },
    },
    {
      name: 'browserDAO',
      documentation: `A DAO that contains browser names, versions
        OS, OS versions and its browser key.`,
      factory: function() {
        return this.EasyDAO.create({
          name: 'browserDAO',
          of: this.Browser,
          daoType: 'MDAO',
        });
      },
    },
    {
      name: 'interfaceDAO',
      documentation: `A DAO that contains interface names, API names,
        and its interface key`,
      factory: function() {
        return this.EasyDAO.create({
          name: 'interfaceDAO',
          of: this.WebInterface,
          daoType: 'MDAO',
        });
      },
    },
    {
      name: 'versionHistory',
      documentation: `An helper gets release date for a brwoser version.`,
      factory: function() {
        return this.VersionHistory.create({
          browserHistory: require('../../data/version_history.json'),
        });
      },
    },
  ],
  methods: [
    {
      name: 'import',
      documentation: `A synchronous function to import interface/API from
        web catalog for a given version of browser.`,
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
          documentation: `The web catalog of this version of
            browser, tested on the given operating system and version.
            The json object should be the JSON object returned from
            com.web.catalog.ApiExtractor`,
          typeName: 'JSON',
        },
      ],
      code: function(browserName, browserVersion,
                     osName, osVersion, apiCatalog) {
        let promises = [];
        let browser = this.Browser.create({
          browserName,
          browserVersion,
          osName,
          osVersion,
          releaseDate: this.versionHistory
              .getReleaseDate(browserName, browserVersion),
        });
        promises.push(this.browserDAO.put(browser));
        let interfaceNames = Object.keys(apiCatalog);
        for (let i = 0; i < interfaceNames.length; i++) {
          let interfaceName = interfaceNames[i];
          for (let j = 0; j < apiCatalog[interfaceName].length; j++) {
            let apiName = apiCatalog[interfaceName][j];
            let webInterface = this.WebInterface.create({
              interfaceName,
              apiName,
            });
            promises.push(this.interfaceDAO.put(webInterface));
            promises.push(webInterface.browsers.put(browser));
          }
        }
        return Promise.all(promises);
      },
    },
  ],
});
