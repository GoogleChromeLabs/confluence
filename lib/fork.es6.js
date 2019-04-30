// Copyright 2018 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

foam.CLASS({
  name: 'ForkBoxFactory',
  package: 'org.chromium.apis.web',

  requires: ['foam.box.node.ForkBox'],

  methods: [
    function getForkBox(mode, ctx) {
      return this.ForkBox.create({
        critical: true,
        nodeParams: this.getForkNodeParams_(),
        childScriptPath: this.getForkScriptPath_(),
        childScriptParams: [mode.toString()],
      }, ctx);
    },
    function getForkNodeParams_() {
      return ['--max_old_space_size=4096'];
    },
    function getForkScriptPath_() {
      return require('path').resolve(`${__dirname}/../../main/forkScript.js`);
    },
  ],
});

foam.CLASS({
  name: 'ForkBoxFactorySingleton',
  package: 'org.chromium.apis.web',
  extends: 'org.chromium.apis.web.ForkBoxFactory',

  axioms: [foam.pattern.Singleton.create()],
});
