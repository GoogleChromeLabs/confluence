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
const FOAM_BIN_TMP_DIR = `${ROOT_DIR}/.local`;
const FOAM_BIN_TMP_PATH = `${FOAM_BIN_TMP_DIR}/foam-bin.js`;
const FOAM_BIN_REG_EXP = /foam-bin\.js$/;
const ES6_REG_EXP = /\.es6\.js$/;

module.exports = {
  ROOT_DIR,
  FOAM_DIR,
  BUNDLE_PROJECT_DIR,
  BUNDLE_DIR,
  FOAM_FLAGS,
  FOAM_BIN_TMP_DIR,
  FOAM_BIN_TMP_PATH,
  FOAM_BIN_REG_EXP,
  ES6_REG_EXP,
};
