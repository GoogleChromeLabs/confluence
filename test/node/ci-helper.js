// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

var process = require('process');

// Tests that are only run locally (not on Travis CI bots).
global.isLocal = !(process.env.TRAVIS && process.env.CI);
global.describeLocal = global.isLocal ? describe : xdescribe;
