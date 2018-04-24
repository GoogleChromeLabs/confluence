// Copyright 2018 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

foam.CLASS({
  name: 'ToggleListElementsView',
  package: 'org.chromium.apis.web',
  extends: 'foam.u2.Element',

  documentation: `A view for toggling a list of arbitrary elements. Similar to
      foam.u2.view.EditColumns view, but with a more consistent treatment of
      list elements and the slots that bind them.

      Elements in the selectable and selected lists are identified by their name
      and use their label for rendering in UI. As such, list elements must each
      have both a unique name and a label.`,

  requires: [
    'foam.u2.md.CheckBox'
  ],
  imports: ['error'],
  exports: [
    'selectable',
    'selected',
  ],

  css: `
    ^item {
      display: flex;
    }

    ^ .foam-u2-md-CheckBox {
      flex-shrink: 0;
    }

    ^ .foam-u2-md-CheckBox-label {
      position: static;
    }
  `,

  classes: [
    {
      name: 'ElementSlot',
      extends: 'foam.core.Slot',

      documentation: `A custom foam.core.Slot implementation; each manages one
          element in the selected collection.`,

      imports: [
        'selectable',
        'selected',
      ],

      properties: [
        {
          class: 'Int',
          name: 'selectableIdx',
        },
      ],

      methods: [
        function get() {
          const elem = this.selectable[this.selectableIdx];
          return this.selected.some(c => c.name === elem.name);
        },
        function set(value) {
          const prevValue = this.get();
          if (value) {
            this.addElement();
          } else {
            this.removeElement();
          }
          // Data binding depnds on an event being published on set. This is
          // automatically taken care of by property slots (with the
          // propertyChange event), but must be published manually for this
          // custom foam.core.Slot implementation.
          if (prevValue !== value) {
            this.pub('elementChange', this.selectableIdx, this);
          }
        },
        function getElement() {
          return this.selectable[this.selectableIdx];
        },
        function addElement() {
          const elem = this.selectable[this.selectableIdx];
          const able = this.selectable;
          const ed = this.selected;
          let iAble = 0;
          let iEd = 0;

          // Protect against multiple add:
          ed.forEach(edElem => foam.assert(
              edElem.name !== elem.name,
              'ElementSlot: Repeated addElement(e)'));

          // Assumptions:
          // (a) Selected is a subset of selectable in the same order;
          // (b) this.selectableIdx is a valid index into selectable.
          //
          // Iterate over selectable, advancing in selected when elements match,
          // until condition (1) or (2) reached. Under above assumptions this
          // will maintain the same-order-subset property.
          while (iAble <= this.selectableIdx) {
            if (iEd >= ed.length) {
              // (1) End of selected is reached: append to selected.
              ed.push(elem);
              break;
            }
            if (iAble === this.selectableIdx) {
              // (2) selectableIdx is reached: splice into selected.
              ed.splice(iEd, 0, elem);
              break;
            }

            if (able[iAble].name === ed[iEd].name) iEd++;
            iAble++;
          }
          this.selected = Array.from(ed);
        },
        function removeElement() {
          const elem = this.selectable[this.selectableIdx];
          const ed = this.selected;
          for (let i = 0; i < ed.length; i++) {
            if (ed[i].name === elem.name) {
              ed.splice(i, 1);
              i--;
            }
          }
          this.selected = Array.from(ed);
        },
      ],
    },
  ],

  properties: [
    {
      class: 'Array',
      name: 'selectable',
      postSet: function(_, nu) {
        for (const elem of this.selectable) {
          if (!elem || !elem.name || !elem.label) {
            this.error(`${this.cls_.id}: Selectable element must have name and
                            label`);
          }
        }
      },
    },
    {
      class: 'Array',
      name: 'selected',
      postSet: function(_, nu) {
        for (const elem of this.selectable) {
          if (!elem || !elem.name || !elem.label) {
            this.error(`${this.cls_.id}: Selected element must have name and
                            label`);
          }
        }
      },
    },
    {
      class: 'FObjectArray',
      of: 'foam.u2.md.CheckBox',
      name: 'uiElements_',
    },
    {
      class: 'Array',
      name: 'slots_',
    },
    {
      class: 'Array',
      name: 'subs_',
    },
  ],

  methods: [
    function init() {
      this.SUPER();
      this.selectable$.sub(this.onSelectableChanged);
    },
    function initE() {
      this.addClass(this.myClass());
      this.renderUIElements();
    },
    {
      name: 'clearUIElements',
      documentation: `Remove child UI elements and detach their associated
          slots.`,
      code: function() {
        for (let i = 0; i < this.uiElements_.length; i++) {
          this.removeChild(this.uiElements_[i]);
          this.subs_[i].detach();
        }
        this.slots_ = [];
        this.uiElements_ = [];
        this.subs_ = [];
      },
    },
    {
      name: 'populateUIElements',
      documentation: `Insert child UI elements, bound to newly created slots.`,
      code: function() {
        for (let i = 0; i < this.selectable.length; i++) {
          const elem = this.selectable[i];
          const slot = this.ElementSlot.create({selectableIdx: i});
          const cb = this.CheckBox.create({
            label: elem.label,
          });
          const sub = cb.data$.linkFrom(slot);
          const wrapper = this.E('div').addClass(this.myClass('item')).add(cb);

          this.slots_.push(slot);
          this.uiElements_.push(wrapper);
          this.subs_.push(sub);

          this.add(wrapper);
        }
      },
    },
    {
      name: 'renderUIElements',
      documenation: 'Force redraw of child elements.',
      code: function() {
        if (this.uiElements_.length !== 0) this.clearUIElements();
        this.populateUIElements();
      },
    },
  ],

  listeners: [
    {
      name: 'onSelectableChanged',
      documentation: 'Respond to change in selectable collection.',
      code: function() { this.renderUIElements(); },
    },
    {
      name: 'removeElement',
      documentation: `Respond to external removal of element from selected
          collection.`,
      code: function (element) {
        for (const slot of this.slots_) {
          if (slot.getElement().name === element.name) {
            slot.set(false);
          }
        }
      },
    },
  ],
});
