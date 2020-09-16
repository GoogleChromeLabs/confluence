// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

const path = require('path');

const {merge} = require('webpack-merge');

const C = require('./webpack.constants.js');
const common = require('./webpack.common.js');

const execSync = require('child_process').execSync;
execSync(`mkdir -p "${C.FOAM_BIN_TMP_DIR}"`);
execSync(`node '${C.FOAM_DIR}/tools/build.js'  ${C.FOAM_FLAGS} "${C.FOAM_BIN_TMP_PATH}"`);

module.exports = merge(common, {
  entry: {
    ui_test: [path.resolve(C.ROOT_DIR, 'main/ui_test.es6')],
  },
  devtool: 'inline-source-map',
  mode: 'development',
});
