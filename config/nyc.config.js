// Copyright 2019 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

const path = require('path');

module.exports = {
  'reporter': ['lcov', 'html', 'json'],
  'report-dir': path.resolve(__dirname, '../.node_coverage'),
};
