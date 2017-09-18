// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

foam.CLASS({
  package: 'org.chromium.apis.web',
  name: 'Handler',
  extends: 'foam.net.node.Handler',

  imports: ['warn'],

  methods: [
    function handle(req, res) {
      const ret = this.canHandleRequest(req);
      if (ret) this.handleRequest(req, res);

      return true;
    },
    function canHandleRequest(req) {
      this.warn('Abstract Handler.canHandleRequest() call');
      return false;
    },
    function handleRequest(req, res) {
      this.warn('Abstract Handler.handleRequest() call');
      return Promise.resolve();
    },
  ],
});
