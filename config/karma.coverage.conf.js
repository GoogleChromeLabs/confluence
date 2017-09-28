// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

// Run all tests in Karma.

const path = require('path');

const base = require('./karma.conf.js');

// Prepend istanbul-instrumenter-loader to default loaders.
let webpack = base.webpackConfig;

webpack.module.rules = [
  {
    test: /\.js$/,
    use: {loader: 'istanbul-instrumenter-loader'},
    include: path.resolve(__dirname, '../lib/'),
  },
].concat(webpack.module.rules);

const files = base.deps
  .concat(base.helpers)
  .concat(base.units)
  .concat(base.integrations);
const preprocessors = {
  'browser/webpack-helper.js': ['webpack'],
};

module.exports = function(config) {
  base(config);
  config.set({
    files,
    // Report coverage gathered by istanbul-instrumenter-loader.
    reporters: (base.reporters || []).concat(['coverage-istanbul']),
    preprocessors,
    webpack,
    // Configure coverage-istanbul.
    coverageIstanbulReporter: {
      reports: ['lcov', 'html', 'json'],
      dir: path.resolve(__dirname, '../.web_coverage'),
      fixWebPackSourcePaths: true,
    },
  });
};
