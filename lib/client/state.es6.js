// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

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
