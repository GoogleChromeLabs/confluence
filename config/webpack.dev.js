// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

const merge = require('webpack-merge');

const C = require('./webpack.constants.js');
const common = require('./webpack.common.js');

const execSync = require('child_process').execSync;
execSync(`node '${C.FOAM_DIR}/tools/build.js'  ${C.FOAM_FLAGS} "${C.FOAM_BIN_TMP_PATH}"`);

module.exports = merge(common, {
  devtool: 'inline-source-map',
  module: {
    rules: [
      {
        test: C.ES6_REG_EXP,
        use: [
          {
            loader: 'babel-loader',
            options: C.ES6_LOADER_OPTIONS_DEV,
          },
        ],
      },
    ],
  },
});
