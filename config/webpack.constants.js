// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

const path = require('path');

const ROOT_DIR = path.resolve(__dirname, '..');
const FOAM_DIR = path.resolve(__dirname, '../node_modules/foam2');
const BUNDLE_PROJECT_DIR = 'static/bundle';
const BUNDLE_DIR = path.resolve(__dirname, `../${BUNDLE_PROJECT_DIR}`);
const FOAM_FLAGS = 'web,gcloud';
const FOAM_BIN_TMP_PATH = `${ROOT_DIR}/.local/foam-bin.js`;
const FOAM_BIN_REG_EXP = /foam-bin\.js$/;
const ES6_REG_EXP = /\.es6\.js$/;
const ES6_LOADER_OPTIONS_PROD = {
  plugins: [
    // presets: ['es2015'] without strict mode:
    'check-es2015-constants',
    'transform-es2015-arrow-functions',
    'transform-es2015-block-scoped-functions',
    'transform-es2015-block-scoping',
    'transform-es2015-classes',
    'transform-es2015-computed-properties',
    'transform-es2015-destructuring',
    'transform-es2015-duplicate-keys',
    'transform-es2015-for-of',
    'transform-es2015-function-name',
    'transform-es2015-literals',
    'transform-es2015-object-super',
    'transform-es2015-parameters',
    'transform-es2015-shorthand-properties',
    'transform-es2015-spread',
    'transform-es2015-sticky-regex',
    'transform-es2015-template-literals',
    'transform-es2015-typeof-symbol',
    'transform-es2015-unicode-regex',
    'transform-regenerator',
    'transform-es2015-block-scoping',
    'transform-es2015-arrow-functions',
    'transform-es2015-template-literals',
    ['transform-es2015-modules-commonjs', {strict: false}],
  ],
  // No, really. Without strict mode.
  parserOpts: {strictMode: false},
};
const ES6_LOADER_OPTIONS_DEV = {
  plugins: ES6_LOADER_OPTIONS_PROD.plugins.concat(['transform-runtime']),
  parserOpts: Object.assign({}, ES6_LOADER_OPTIONS_PROD.parserOpts),
};

module.exports = {
  ROOT_DIR,
  FOAM_DIR,
  BUNDLE_PROJECT_DIR,
  BUNDLE_DIR,
  FOAM_FLAGS,
  FOAM_BIN_TMP_PATH,
  FOAM_BIN_REG_EXP,
  ES6_REG_EXP,
  ES6_LOADER_OPTIONS_PROD,
  ES6_LOADER_OPTIONS_DEV,
};
