// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

// Load external libraries and data.
let libs = {
  ObjectGraph: require('object-graph-js').ObjectGraph,
  DATA: {
	chrome56: require('../data/window_Chrome_56_Windows_10.0.json'),
	edge14: require('../data/window_Edge_14_Windows_10.0.json'),
	safari602: require('../data/window_Safari_602_OSX_10.12.3.json'),
  },
};

Object.keys(libs).forEach((key) => {
  global[key] = libs[key];
});
