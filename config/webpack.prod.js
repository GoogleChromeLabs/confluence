// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

const merge = require('webpack-merge');
const TerserPlugin = require('terser-webpack-plugin');

const C = require('./webpack.constants.js');
const common = require('./webpack.common.js');

const execSync = require('child_process').execSync;
execSync(`mkdir -p "${C.FOAM_BIN_TMP_DIR}"`);
execSync(`node '${C.FOAM_DIR}/tools/build.js'  ${C.FOAM_FLAGS} "${C.FOAM_BIN_TMP_PATH}"`);

module.exports = merge(common, {
  devtool: false,
  mode: 'production',
  module: {
    rules: [
      {
        test: [C.ES6_REG_EXP, C.FOAM_BIN_REG_EXP],
        use: [
          {
            loader: 'babel-loader',
            options: {
              presets: ['@babel/preset-env'],
              // No, really. Without strict mode.
              parserOpts: {strictMode: false},
            },
          },
        ],
      },
    ],
  },
  optimization: {
    minimizer: [
      new TerserPlugin({
        terserOptions: {
          keep_fnames: true,
          mangle: false,
          output: {comments: false},
        },
      }),
    ],
  },
  performance: {
    // These size limits are *much* larger than webpack's default
    // recommendation of 250kb, but are combined with 'error' to ensure that
    // they don't grow accidentally beyond the current size.
    hints: 'error',
    maxAssetSize: 1.1*1024*1024,
    maxEntrypointSize: 1.8*1024*1024,
  },
});
