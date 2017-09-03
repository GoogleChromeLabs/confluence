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
      name: 'matrix',
    },
  ],
});
