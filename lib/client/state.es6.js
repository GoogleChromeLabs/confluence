// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

// Refine foam.Object.equals to recursively check own object keys. This is
// necessary for dealing with plain objects in URL state comparisons.
foam.LIB({
  name: 'foam.Object',

  methods: [
    function equals(a, b) {
      var keys = {};
      if ( ! foam.Object.isInstance(b) ) return false;
      for (let key in a) {
        if (!a.hasOwnProperty(key)) continue;
        if (!foam.util.equals(a[key], b[key])) return false;
        keys[key] = true;
      }
      for (let key in b) {
        if (keys[key] || !b.hasOwnProperty(key)) continue;
        if (!foam.util.equals(a[key], b[key])) return false;
      }
      return true;
    },
  ],
});

// Provide a foam.core.internal.PropertySlot implementation for plain objects.
foam.CLASS({
  name: 'PropertySlot',
  package: 'org.chromium.apis.web',
  extends: 'foam.core.Slot',

  properties: [
    {
      class: 'String',
      name: 'name',
      documentation: 'The property name where the slot should be installed.',
      required: true,
    },
    {
      name: 'object',
      documentation: 'The object containing the named value.',
      required: true,
    },
    {
      name: 'value_',
      documentation: 'Value currently stored in property.'
    },
  ],

  methods: [
    function init() {
      this.SUPER();
      this.validate();

      const initialValue = this.object[this.name];
      Object.defineProperty(this.object, this.name, {
        get: () => this.get(),
        set: newValue => this.set(newValue),
      });
      this.set(initialValue);
    },
    function get() { return this.value_; },
    function set(newValue) { return this.value_ = newValue; },
  ]
});
