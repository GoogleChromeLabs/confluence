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
      class: 'String',
      name: 'id',
      documentation: 'Identifier for view associated with matrix.',
      required: true,
    },
    {
      name: 'matrix',
      documentation: 'Latest matrix data for view.',
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
