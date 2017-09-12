// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

foam.CLASS({
  package: 'org.chromium.apis.web',
  name: 'Event',
});

foam.CLASS({
  package: 'org.chromium.apis.web',
  name: 'NewMatrixEvent',
  extends: 'org.chromium.apis.web.Event',

  properties: [
    {
      class: 'Array',
      of: 'String',
      name: 'releaseKeys',
      required: true,
    },
    {
      class: 'String',
      name: 'searchKey',
    },
    {
      name: 'releaseOptions',
      value: null,
    },
    {
      name: 'numAvailable',
      value: null,
    },
    {
      name: 'matrix',
      required: true,
    },
  ],

  methods: [
    function init() {
      this.SUPER();
      this.validate();
    },
  ],
});

foam.CLASS({
  package: 'org.chromium.apis.web',
  name: 'DataLoadedEvent',
  extends: 'org.chromium.apis.web.Event',
});
