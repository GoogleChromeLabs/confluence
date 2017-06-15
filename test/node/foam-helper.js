// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

beforeAll(function() {
  var path = require('path');

  global.FOAM_FLAGS = {gcloud: true};
  require(path.resolve(__dirname, '..', '..', 'node_modules', 'foam2', 'src',
                       'foam.js'));
});
