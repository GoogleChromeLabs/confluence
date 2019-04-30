// Copyright 2018 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

require('./u2/ActionView.es6.js');

foam.CLASS({
  refines: 'foam.core.Action',

  requires: [
    'org.chromium.apis.web.ActionView',
  ],

  methods: [
    function toE(args, X) {
      const view = foam.u2.ViewSpec.createView({
        class: 'org.chromium.apis.web.ActionView',
        action: this,
      }, args, this, X);

      if (X.data$ && !(args && (args.data || args.data$))) {
        view.data$ = X.data$;
      }

      return view;
    },
  ],
});
