// Copyright 2018 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

require('./ActionView.es6.js');
require('./SearchView.es6.js');
require('./ToggleListElementsView.es6.js');

foam.CLASS({
  name: 'DAOControllerView',
  package: 'org.chromium.apis.web',
  extends: 'foam.u2.View',
  implements: ['foam.mlang.Expressions'],

  requires: [
    'foam.u2.md.OverlayDropdown',
    'org.chromium.apis.web.ActionView',
    'org.chromium.apis.web.SearchView',
    'org.chromium.apis.web.ToggleListElementsView',
  ],
  imports: [
    'data? as importedData',
    'selectable? as importedSelectableColumns',
    'selected? as importedSelectedColumns',
  ],
  exports: [
    'selectable', // For query parser.
    'selected', // For query parser.
    'selected as columns', // For DAO view.
    'removeColumn', // For daoView.
  ],

  css: `
    ^ {
      flex-grow: 1;
      flex-shrink: 1;
      min-width: 650px;
      display: flex;
      min-height: 0;
      flex-direction: column;
      font-size: 14px;
    }

    ^controls {
      flex-grow: 0;
      flex-shrink: 0;
      display: flex;
      min-height: 0;
      background-color: rgb(29, 103, 189);
      color: rgba(255, 255, 255, 0.8);
      box-shadow: 0 2px 2px 0 rgba(0, 0, 0, 0.14), 0 1px 5px 0 rgba(0, 0, 0, 0.12), 0 3px 1px -2px rgba(0, 0, 0, 0.2);
      z-index: 30;
    }

    ^columns-container {
      position: relative;
    }

    ^DAOView {
      flex-grow: 1;
      z-index: 1;
    }

    ^count {
      z-index: 20;
      flex-grow: 0;
      flex-shrink: 0;
      display: flex;
      min-height: 0;
      justify-content: flex-end;
      padding: 1rem;
      background-color: rgb(145, 200, 246);
      box-shadow: 0 -2px 2px 0 rgba(0, 0, 0, 0.14), 0 -1px 5px 0 rgba(0, 0, 0, 0.12), 0 -3px 1px -2px rgba(0, 0, 0, 0.2);
    }

    ^icon {
      z-index: 2;
      display: inline-block;
      padding: 1rem;
    }

    ^button {
      flex-grow: 0;
      flex-shrink: 0;
      background-color: inherit;
      color: inherit;
      -webkit-user-select: none;
      -moz-user-select: none;
      -ms-user-select: none;
      user-select: none;
    }

    ^button:hover {
      background-color: rgba(255, 255, 255, 0.2);
      cursor: pointer;
    }

    ^ .foam-u2-md-OverlayDropdown {
      width: 300px;
    }

    ^search {
      flex-grow: 1;
      z-index: 3;
    }
  `,

  properties: [
    {
      class: 'FObjectProperty',
      of: 'org.chromium.apis.web.DAOController',
      name: 'data',
      expression: function(importedData) { return importedData; },
      required: true,
    },
    {
      class: 'Class',
      name: 'of',
      // TODO(markdittmer): This should work as an expression, but doesn't.
      factory: function() { return this.data.data.of; },
    },
    {
      class: 'FObjectArray',
      of: 'foam.core.Property',
      name: 'selectable',
      factory: function() {
        const imported = this.importedSelectableColumns;
        if (Array.isArray(imported) && imported.length > 0 &&
            foam.core.Property.isInstance(imported[0])) {
          return imported;
        }

        const tableColumns = this.of.getAxiomByName('tableColumns');
        if (tableColumns.columns) {
          return tableColumns.columns.map(name => this.of.getAxiomByName(name));
        }

        return this.of.getAxiomsByClass(foam.core.Property)
            .filter(prop => !prop.hidden);
      },
    },
    {
      class: 'FObjectArray',
      of: 'foam.core.Property',
      name: 'selected',
      factory: function() {
        return this.importedSelectedColumns ||
            Array.from(this.selectable);
      },
    },
    {
      name: 'columnSelectionE',
      // TODO(markdittmer): Should this be a ViewSpec instead of a view?
      factory: function() {
        return this.OverlayDropdown.create().add(this.columnSelectionView_);
      },
    },
    {
      name: 'columnSelectionView_',
      // TODO(markdittmer): Should this be a ViewSpec instead of a view?
      factory: function() {
        return this.ToggleListElementsView.create({
          selectable: this.selectable,
          selected$: this.selected$,
        });
      },
    },
    {
      name: 'columnsIconE',
      // TODO(markdittmer): Should this be a ViewSpec instead of a view?
      factory: function() {
        return this.E('i').addClass('material-icons').add('more_vert')
          .addClass(this.myClass('icon')).addClass(this.myClass('button'))
          .on('click', () => {
            this.columnSelectionE.open();
          });
      },
    },
    {
      class: 'foam.u2.ViewSpec',
      name: 'searchSpec',
      value: {class: 'org.chromium.apis.web.SearchView'},
    },
    {
      name: 'searchE',
    },
    {
      class: 'Int',
      name: 'count_',
    },
    {
      class: 'Int',
      name: 'total_',
    },
  ],

  methods: [
    function init() {
      this.SUPER();
      this.data$.dot('data').sub(this.onDAOChange);
      this.data$.dot('filteredDAO').sub(this.onFilteredDAOChange);
      this.onDAOChange();
      this.onFilteredDAOChange();
    },
    function initE() {
      let daoView = this.data.FILTERED_DAO.toE(null, this);
      daoView.addClass(this.myClass('DAOView'));
      let searchView = foam.u2.ViewSpec.createView(
          this.searchSpec, {}, this, this.__subSubContext__)
          .addClass(this.myClass('search'));
      this.addClass(this.myClass())
          .start('div').addClass(this.myClass('controls'))
          .add(searchView)
          .add(this.slot(data => {
            return this.E('actions')
                .add(data.cls_.getAxiomsByClass(foam.core.Action));
          }, this.data$))
          .start('span').addClass(this.myClass('columns-container'))
          .add(this.columnsIconE)
          .add(this.columnSelectionE)
          .end()
          .end()
          .add(daoView)
          .start('div').addClass(this.myClass('count'))
          .add(this.count_$)
          .entity('nbsp').add('/').entity('nbsp')
          .add(this.total_$);
    },
  ],

  listeners: [
    function onDAOChange() {
      this.data && this.data.data.select(this.COUNT())
          .then(countSink => this.total_ = countSink.value);
    },
    function onFilteredDAOChange() {
      this.data && this.data.filteredDAO.select(this.COUNT())
          .then(countSink => this.count_ = countSink.value);
    },
    function removeColumn(column) {
      return this.columnSelectionView_.removeElement(column);
    },
  ],
});
