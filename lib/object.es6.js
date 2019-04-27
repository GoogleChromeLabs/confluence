// Copyright 2018 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

// Refine foam.Object.equals to recursively check own object keys. This is
// necessary for dealing with plain objects in URL state comparisons.
foam.LIB({
  name: 'foam.Object',

  methods: [
    function equals(a, b) {
      const keys = {};
      if ( ! foam.Object.isInstance(b) ) return false;
      for (const key in a) {
        if (!a.hasOwnProperty(key)) continue;
        if (!foam.util.equals(a[key], b[key])) return false;
        keys[key] = true;
      }
      for (const key in b) {
        if (keys[key] || !b.hasOwnProperty(key)) continue;
        if (!foam.util.equals(a[key], b[key])) return false;
      }
      return true;
    },
  ],
});
