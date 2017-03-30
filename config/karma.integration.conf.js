// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

// Run integration tests in Karma.
const base = require('./karma.conf.js');
const webpack = base.webpackConfig;
const files = base.deps
  .concat(base.helpers)
  .concat(base.integrations);

module.exports = function(config) {
  base(config);
  config.set({
    files,
    webpack,
  });
};
