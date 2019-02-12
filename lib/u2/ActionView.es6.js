// Copyright 2018 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

foam.CLASS({
  name: 'ActionView',
  package: 'org.chromium.apis.web',
  extends: 'foam.u2.Element',

  css: `
    ^ {
      position: relative;
    }

    ^label {
      color: inherit;
      -webkit-user-select: none;
      -moz-user-select: none;
      -ms-user-select: none;
      user-select: none;
      display: inline-block;
      padding: 1rem;
    }

    ^label:hover {
      background-color: rgba(255, 255, 255, 0.2);
      cursor: pointer;
    }

    ^unavailable {
      display: none;
    }

    ^disabled {
      opacity: 0.5;
    }

    /*
    * Old behaviour: Display greyed-out disabled buttons
    *
    ^disabled, ^disabled^label:hover {
      filter: opacity(0.4);
      background-color: inherit;
    }
    */
  `,

  properties: [
    'data',
    'action',
    {
      name: 'icon',
      expression: function(action) { return action.icon || action.name; }
    }
  ],

  methods: [
    function initE() {
      let label = this.E('i').addClass('material-icons')
          .addClass(this.myClass('label'))
          .add(this.icon);

      this.nodeName = 'span';
      this.addClass(this.myClass()).addClass(this.myClass(this.action.name))
          .add(label).on('click', this.onClick);

      if (this.action.isAvailable) {
        label.enableClass(this.myClass('unavailable'),
                          this.action.createIsAvailable$(this.data$),
                          true /* negate */);
      }
      if (this.action.isEnabled) {
        const isEnabled = this.action.createIsEnabled$(this.data$);
        this.attrs({
          disabled: isEnabled.map(function(e) {
            return e ? false : 'disabled';
          })
        });
        label.enableClass(this.myClass('disabled'),
                          isEnabled,
                          true /* negate */);
      }
    },
  ],

  listeners: [
    function onClick() {
      this.action.maybeCall(this.__subContext__, this.data);
    }
  ]
});
