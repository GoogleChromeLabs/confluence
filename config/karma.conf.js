// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

const FOAM_DIR = `${__dirname}/../node_modules/foam2`;
const execSync = require('child_process').execSync;
execSync(`node ${FOAM_DIR}/tools/build.js web`);

// Run all tests in Karma.

const basePath = `${__dirname}/../test`;

const files = [
  `${__dirname}/../node_modules/foam2/foam-bin.js`,
];
const entries = [
  '../main/*.js',
];
const helpers = [
  'any/**/*-helper*.js',
  'browser/**/*-helper*.js',
  '../node_modules/foam2/test/helpers/testcontext.js',
];
const units = [
  'any/**/*-test*.js',
  'browser/**/*-test*.js',
];
const integrations = [
  'any/**/*-integration*.js',
  'browser/**/*-integration*.js',
];
const reporters = ['progress'];
const preprocessors = {
  'any/files-helper.js': ['webpack'],
  'any/data-helper.js': ['webpack'],
};

function configurator(config) {
  config.set({
    // base path that will be used to resolve all patterns (eg. files, exclude)
    basePath,

    // frameworks to use
    // available frameworks: https://npmjs.org/browse/keyword/karma-adapter
    frameworks: ['jasmine'],

    // list of files / patterns to load in the browser
    files,

    // test results reporter to use
    // possible values: 'dots', 'progress'
    // available reporters: https://npmjs.org/browse/keyword/karma-reporter
    reporters,

    // preprocess matching files before serving them to the browser available
    // preprocessors: https://npmjs.org/browse/keyword/karma-preprocessor
    preprocessors,

    // web server port
    port: 9876,

    // enable / disable colors in the output (reporters and logs)
    colors: true,

    // level of logging
    // possible values: config.LOG_DISABLE || config.LOG_ERROR ||
    // config.LOG_WARN || config.LOG_INFO || config.LOG_DEBUG
    logLevel: config.LOG_INFO,

    // start these browsers
    // available browser launchers:
    // https://npmjs.org/browse/keyword/karma-launcher
    // If run in travisCI, use firefox, otherwise, use Chrome.
    browsers: process.env.TRAVIS ? ['Firefox'] : ['Chrome'],

    // Continuous Integration mode
    // if true, Karma captures browsers, runs the tests and exits
    singleRun: true,

    // enable / disable watching file and executing tests whenever any file
    // changes
    autoWatch: false,

    // Concurrency level
    // how many browser should be started simultaneous
    concurrency: Infinity,

    captureTimeout: 150000,
    browserDisconnectTimeout: 100000,
    browserDisconnectTolerance: 5,
    browserNoActivityTimeout: 150000,
  });
};

configurator.srcGlobs = [
  '../lib/**/*.js',
];

const C = require('./webpack.constants.js');
configurator.webpackConfig = {
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
      {
        test: /\.html$/,
        use: [{loader: 'html-loader'}],
      },
      {
        test: /\.css$/,
        use: [{loader: 'style-loader'}, {loader: 'css-loader'}],
      },
      {
        test: /\.(ttf|eot|svg|woff(2)?)(\?[a-z0-9=&.]+)?$/,
        loader: 'file-loader?name=fonts/[name].[ext]',
      },
    ],
  },
  node: {
    fs: 'empty',
    dns: 'empty',
  },
};

configurator.deps = files;
configurator.entries = entries;
configurator.helpers = helpers;
configurator.units = units;
configurator.integrations = integrations;

module.exports = configurator;
