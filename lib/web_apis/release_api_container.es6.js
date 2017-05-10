// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

require('./release.es6.js');
require('./release_interface_relationship.es6.js');
require('./web_interface.es6.js');

foam.CLASS({
  name: 'ReleaseApiContainer',
  package: 'org.chromium.apis.web',

  documentation: `Container for contextualizing related DAOs:

  Release,
  WebInterface,
  Release <--> WebInterface (ReleaseWebInterfaceJunction)`,

  exports: ['releaseDAO', 'webInterfaceDAO', 'releaseWebInterfaceJunctionDAO'],

  properties: [
    {
      class: 'FObjectProperty',
      of: 'foam.dao.DAO',
      name: 'releaseDAO',
      documentation: `A DAO containing all known browser releases.`,
      required: true,
      final: true,
    },
    {
      class: 'FObjectProperty',
      of: 'foam.dao.DAO',
      name: 'webInterfaceDAO',
      documentation: `A DAO containing all known interface and API pairs.`,
      required: true,
      final: true,
    },
    {
      class: 'FObjectProperty',
      of: 'foam.dao.DAO',
      name: 'releaseWebInterfaceJunctionDAO',
      documentation: `A DAO containing junction objects of Release and
          WebInterface.`,
      required: true,
      final: true,
    },
  ]
});
