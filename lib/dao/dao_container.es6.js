// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

foam.CLASS({
  name: 'DAOContainer',
  package: 'org.chromium.apis.web',

  documentation: `Container for contextualizing related DAOs:

  Release,
  WebInterface,
  Release <--> WebInterface (ReleaseWebInterfaceJunction),
  BrowserMetricData (lone removal, browser-specific, etc.),
  ApiCountData (total APIs, new APIs, removed APIs).`,

  exports: [
    'as container',
    'apiCountDAO',
    'browserMetricsDAO',
    'compatDAO',
    'releaseDAO',
    'releaseWebInterfaceJunctionDAO',
    'webInterfaceDAO',
  ],

  // Names applied to DAOs on both ends of WebSocket. These names are
  // consolidated here to ensure that they match on the web client and front end
  // server.
  constants: {
    // TODO(markdittmer): Drop rel/api/junction DAOs once client is fully ported
    // to generated.CompatData model.
    RELEASE_NAME: 'releaseDAO',
    WEB_INTERFACE_NAME: 'webInterfaceDAO',
    RELEASE_WEB_INTERFACE_JUNCTION_NAME: 'releaseWebInterfaceJunctionDAO',
    COMPAT_NAME: 'compatDAO',
    COMPAT_MODEL_FILE_NAME: 'class:org.chromium.apis.web.generated.CompatData.json',
    API_COUNT_NAME: 'apiCountDAO',
    LONE_OMISSION_NAME: 'loneOmissionDAO',
    BROWSER_SPECIFIC_NAME: 'browserSpecificDAO',
    LONE_REMOVAL_NAME: 'loneRemovalDAO',

    // Names of DAOs that the client will cache in workers.
    CLIENT_CACHED_NAMES: {
      'compatDAO': true,
    },
  },

  properties: [
    {
      class: 'foam.dao.DAOProperty',
      documentation: 'DAO providing access to all known browser releases.',
      name: 'releaseDAO',
    },
    {
      class: 'foam.dao.DAOProperty',
      documentation: 'DAO providing access to all known web APIs.',
      name: 'webInterfaceDAO',
    },
    {
      class: 'foam.dao.DAOProperty',
      documentation: `DAO providing access to all browser release <--> API
          relations.`,
      name: 'releaseWebInterfaceJunctionDAO',
    },
    {
      class: 'foam.dao.DAOProperty',
      documentation: `DAO providing access to unified
          <API, [browser support bools]> compat data model.`,
      name: 'compatDAO',
    },
    {
      class: 'foam.dao.DAOProperty',
      documentation: `DAO providing access to historical browser metrics data.`,
      name: 'browserMetricsDAO',
    },
    {
      class: 'foam.dao.DAOProperty',
      documentation: `DAO providing access to API Count metric data.`,
      name: 'apiCountDAO',
    },
  ],
});
