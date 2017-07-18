// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

require('../web_apis/release_interface_relationship.es6.js');

foam.CLASS({
  package: 'org.chromium.apis.web',
  name: 'ReleaseWebInterfaceJunctionDAO',
  extends: 'foam.dao.IDBDAO',

  documentation: `IDBDAO configured for indexing over
      ReleaseWebInterfaceJunction objects.`,

  properties: [
    {
      name: 'of',
      value: org.chromium.apis.web.ReleaseWebInterfaceJunction
    },
    {
      name: 'indicies',
      value: [
        // Non-unique index on ReleaseWebInterfaceJunction.sourceId:
        ['sourceId', false],
        // Non-unique index on ReleaseWebInterfaceJunction.targetId:
        ['targetId', false],
      ],
    },
  ],
});
