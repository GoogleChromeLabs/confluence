// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

foam.CLASS({
  package: 'org.chromium.apis.web',
  name: 'Event',
});

foam.CLASS({
  package: 'org.chromium.apis.web',
  name: 'NewMatrixEvent',

  properties: [
    {
      name: 'matrix',
    },
  ],
});

foam.LIB({
  name: 'org.chromium.apis.web.events',

  methods: [
    function waitUntil(pub, events) {
      let matched = [];
      return new Promise(function(resolve, reject) {
        pub.sub(function(_, event) {
          const alreadyMatched = matched.filter(
              foam.util.equals.bind(foam.util, event));
          const matches = events.filter(
              foam.util.equals.bind(foam.util, event));
          const newMatches = matches.filter(function(match) {
            return alreadyMatched.filter(
              foam.util.equals.bind(foam.util, match)).length === 0;
          });
          matched = matched.concat(newMatches);
          if (matched.length === events.length) resolve(matched);
        });
      });
    }
  ]
});
