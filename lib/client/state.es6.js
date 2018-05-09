// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

require('../web_apis/api_compat_data.es6.js');

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

foam.CLASS({
  name: 'CatalogState',
  package: 'org.chromium.apis.web',

  documentation: 'Controller for URL state variables on catalog page',

  requires: [
    'foam.web.URLState',
  ],
  imports: ['selectable?'],
  exports: [
    'query',
    'selected',
  ],

  properties: [
    {
      class: 'FObjectProperty',
      of: 'foam.web.DetachedURLState',
      name: 'urlState',
      documentation: 'Page-global URL state to control.',
      factory: function() { return this.URLState.create(); },
    },
    {
      class: 'String',
      documentation: 'Raw query string for catalog view.',
      name: 'query',
    },
    {
      class: 'FObjectArray',
      of: 'org.chromium.apis.web.CompatProperty',
      documentation: 'Compat data properties associated with releases in view.',
      name: 'selected',
      preSet: function(old, nu) {
        if (!this.selectable) return nu;

        const indexOf = (prop) => {
          let ret = -1;
          this.selectable.some((selectableProp, idx) => {
            if (selectableProp.name === prop.name) {
              ret = idx;
              return true;
            }
          });
          return ret;
        }
        const comparator = (prop1, prop2) => {
          return indexOf(prop1) - indexOf(prop2);
        }
        return nu.sort(comparator);
      },
      postSet: function(old, nu) {
        if (foam.util.equals(old, nu)) return;
        this.selectedReleases = nu.map(prop => prop.release.id);
      },
    },
    {
      class: 'Array',
      of: 'String',
      name: 'selectedReleases',
      documentation: 'String representation for binding releases in view.',
      postSet: function(old, nu) {
        if (foam.util.equals(old, nu) || !this.selectable) return;
        this.selected = nu.map(id => {
          const prop = this.selectable.find(prop => prop.release.id === id);
          foam.assert(!foam.Undefined.isInstance(prop),
                      `Unknown release: ${id}`);
          return prop;
        });
      },
    },
  ],

  methods: [
    function init() {
      this.SUPER();
      this.urlState.addBinding('releases',this.selectedReleases$);
      this.urlState.addBinding('q', this.query$);
    },
  ],
});
