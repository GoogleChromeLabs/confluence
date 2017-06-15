// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

require('./release.es6.js');
require('./web_interface.es6.js');

foam.RELATIONSHIP({
  sourceModel: 'org.chromium.apis.web.Release',
  targetModel: 'org.chromium.apis.web.WebInterface',
  forwardName: 'interfaces',
  inverseName: 'releases',
  cardinality: '*:*',
});
