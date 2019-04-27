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

  documentation: `Default view for a DAOController. Renders a search box,
      columns selector, DAO table view and a "n of m" record count: n = number
      of records according to search filter; m = total number of records in
      underlying DAO.`,

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
    'setTimeout',
    'window',
  ],
  exports: [
    'selectable', // For query parser.
    'selected', // For query parser.
    'columns', // For DAO view.
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

    ^columns-container .foam-u2-md-OverlayDropdown {
      font-size: 14px;
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
      documentation: 'The DAOController being rendered by this view.',
      expression: function(importedData) {
        return importedData;
      },
      required: true,
    },
    {
      class: 'Class',
      name: 'of',
      documentation: 'The class of objects stored in the underlying DAO.',
      // TODO(markdittmer): This should work as an expression, but doesn't.
      factory: function() {
        return this.data.data.of;
      },
    },
    {
      class: 'FObjectArray',
      of: 'foam.core.Property',
      name: 'selectable',
      documentation: `The properties that the user can select for rendering in
          the DAO table view.`,
      final: true,
      factory: function() {
        const imported = this.importedSelectableColumns;
        if (Array.isArray(imported) && imported.length > 0 &&
            foam.core.Property.isInstance(imported[0])) {
          return imported;
        }

        const tableColumns = this.of.getAxiomByName('tableColumns');
        if (tableColumns.columns) {
          return tableColumns.columns.map((name) => this.of.getAxiomByName(name));
        }

        return this.of.getAxiomsByClass(foam.core.Property)
            .filter((prop) => !prop.hidden);
      },
    },
    {
      class: 'FObjectArray',
      of: 'foam.core.Property',
      name: 'selected',
      factory: function() {
        // Establish two-way contextual binding iff import for binding exists.
        if (this.importedSelectedColumns) {
          this.setTimeout(
              () => this.selected$.linkFrom(this.importedSelectedColumns$),
              0);
          return this.importedSelectedColumns;
        }

        return Array.from(this.selectable);
      },
      postSet: function(old, nu) {
        // Debounce selected <--> columns binding.
        if (foam.util.equals(old, nu)) return;
        // Columns is always [ID, ...selected...].
        if (Array.isArray(nu)) this.columns = [this.of.ID].concat(nu);
      },
    },
    {
      class: 'FObjectArray',

      of: 'foam.core.Property',
      name: 'columns',
      factory: function() {
        return [this.of.ID].concat(this.selected);
      },
      postSet: function(old, nu) {
        // Debounce selected <--> columns binding.
        if (foam.util.equals(old, nu)) return;
        // Columns is always [ID, ...selected...].
        if (Array.isArray(nu)) this.selected = nu.slice(1);
      },
    },
    {
      name: 'columnSelectionE',
      documentation: `The foam.u2.Element that constitutes the column selection
          view.`,
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
      documentation: `The foam.u2.Element that constitutes the column selection
          action button.`,
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
      documentation: 'The view specification for the search box.',
      value: {class: 'org.chromium.apis.web.SearchView'},
    },
    {
      name: 'searchE',
      documentation: 'The foam.u2.Element that constitutes the search box.',
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
      const daoView = this.data.FILTERED_DAO.toE(null, this);
      daoView.addClass(this.myClass('DAOView'));
      const searchView = foam.u2.ViewSpec.createView(
          this.searchSpec, {}, this, this.__subSubContext__)
          .addClass(this.myClass('search'));
      this.addClass(this.myClass())
          .start('div').addClass(this.myClass('controls'))
          .add(searchView)
          .add(this.slot((data) => {
            return this.E('actions')
                .add(data.cls_.getAxiomsByClass(foam.core.Action)
                    .concat([this.EXPORT_TO_CSV]));
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
          .add(this.total_$)
          .end();
    },
  ],

  actions: [
    {
      name: 'exportToCSV',
      label: 'Export to CSV',
      icon: 'save_alt',
      // For isEnabled and code: "this" bound to "this.data" because view
      // actions operate on view's data. Params on isEnabled bound to same-name
      // params on data model (DAOController).
      isEnabled: function(downloadInProgress) {
        return !downloadInProgress;
      },
      code: function(ctx) {
        const document = ctx.window.document;
        const q = ctx.query;
        const props = Array.from(ctx.selected);

        this.downloadData(ctx).then(({hash, data}) => {
          const filename = `confluence_${Math.abs(hash).toString(16)}.csv`;

          let str = '"' + props.map((p) => p.label)
              .concat(q ? ['', 'Query:', q] : []).join('","') + '",\n';
          for (const datum of data) {
            str += `"${datum.interfaceName}#${datum.apiName}",`;
            for (const prop of props) {
              str += `"${prop.f(datum) ? 'TRUE' : 'FALSE'}",`;
            }
            str += '\n';
          }

          const href = `data:text/csv;charset=utf-8,${encodeURIComponent(str)}`;
          const link = document.createElement('a');
          link.style.display = 'none';
          link.setAttribute('href', href);
          link.setAttribute('download', filename);
          document.body.appendChild(link);
          link.click();
        });
      },
    },
  ],

  listeners: [
    {
      name: 'onDAOChange',
      documentation: `Respond to change in DAOController's DAO; i.e., change to
          this.data.data property.`,
      code: function() {
        this.data && this.data.data.select(this.COUNT())
            .then((countSink) => this.total_ = countSink.value);
      },
    },
    {
      name: 'onFilteredDAOChange',
      documentation: `Respond to change in DAOController's filtered DAO; i.e.,
          change to this.data.filteredDAO property.`,
      code: function() {
        this.data && this.data.filteredDAO.select(this.COUNT())
            .then((countSink) => this.count_ = countSink.value);
      },
    },
    {
      name: 'removeColumn',
      documentation: `Exported routine for synchronizing views that deal with
          columns being removed from the table view. This is a listener to
          ensure that it is bound to this.`,
      code: function(column) {
        return this.columnSelectionView_.removeElement(column);
      },
    },
  ],
});
