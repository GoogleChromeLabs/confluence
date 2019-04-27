// Copyright 2018 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

foam.CLASS({
  name: 'ScrollDAOTable',
  package: 'org.chromium.apis.web',
  extends: 'foam.u2.View',

  documentation: `Decorator for foam.u2.view.ScrollDAOView that adds column
      headers, and the ability to remove columns with a "X" button per column.`,

  requires: [
    'foam.u2.view.ScrollDAOView',
    'org.chromium.apis.web.Property',
  ],
  imports: [
    'columns',
    'removeColumn?',
  ],

  css: `
    ^ {
      display: flex;
      min-height: 0;
      flex-direction: column;
    }

    ^ col-heads {
      z-index: 20;
      flex-grow: 0;
      flex-shrink: 0;
      border-top: 1px solid #d0d0d0;
      overflow-y: scroll;
      background-color: rgb(248, 248, 248);
      font-weight: bold;
      height: 50px;
      box-shadow: 0 1px 2px 0 rgba(0, 0, 0, 0.14), 0 2px 1px -2px rgba(0, 0, 0, 0.2);
      position: relative;
    }

    ^ col-head {
      overflow: hidden;
      display: flex;
      min-height: 0;
      justify-content: space-between;
      align-items: center;
    }

    ^scroll-container {
      position: relative;
      flex-grow: 1;
    }

    ^ .id {
      overflow: hidden;
    }

    ^ .ellipsis {
      height: 100%;
      color: rgba(0, 0, 0, 0.8);
      display: flex;
      align-items: center;
      margin: auto 0;
      padding: 0 5px;
      overflow: hidden;
    }

    ^ .ellipsis span {
      overflow: hidden;
      white-space: nowrap;
      text-overflow: ellipsis;
    }

    ^ .overlay {
      display: none;
      position: absolute;
      margin: auto 0;
      padding: 0 5px;
    }

    ^ li:hover .id {
     color: transparent;
    }

    ^ li:hover .overlay, ^ col-head:hover .overlay {
      height: 100%;
      color: rgba(0, 0, 0, 0.8);
      display: flex;
      align-items: center;
    }

    ^ li:hover .overlay {
      background-color: rgba(255, 255, 255, 0.6);
    }

    ^ col-head, ^ col-head:hover .overlay {
      background-color: inherit;
    }

    ^ col-head:hover .overlay {
      z-index: 3;
    }

    ^ li:hover .ellipsis, ^ col-head:hover .ellipsis {
      visibility: hidden;
    }

    ^icon {
      z-index: 2;
      display: inline-block;
      padding: 1rem;
    }

    ^button {
      flex-grow: 0;
      flex-shrink: 0;
      font-size: inherit;
      background-color: inherit;
      color: inherit;
      padding: 0.5rem 0 0.5rem 0.5rem;
      -webkit-user-select: none;
      -moz-user-select: none;
      -ms-user-select: none;
      user-select: none;
    }

    ^button:hover {
      background-color: rgba(255, 255, 255, 0.2);
      cursor: pointer;
    }
  `,

  classes: [
    {
      name: 'RowFormatter',
      implements: ['foam.u2.RowFormatter'],

      documentation: `Extend foam.u2.RowFormatter to support inversion of
          control for rendering individual table cells.`,

      imports: ['columns'],

      methods: [
        function format(data, opt_columns) {
          const columns = opt_columns || this.columns;

          let str = '';
          for (let i = 0; i < columns.length; i++) {
            const col = columns[i];
            const value = data ? col.f(data) : undefined;
            str += col.rawTableCellFormatter(value, data, col);
          }

          return str;
        },
      ],
    },
  ],

  properties: [
    {
      class: 'foam.u2.ViewSpec',
      name: 'scrollView',
      documentation: `View that actually performs scrolling. Default to
          foam.u2.view.ScrollDAOView with two-way bound "data", reasonable row
          defaults, and a per column/cell row formatter.`,
      factory: function() {
        return {
          class: 'foam.u2.view.ScrollDAOView',
          data$: this.data$,
          numRows: 40,
          rowHeight: 50,
          rowFormatter: this.RowFormatter.create(),
        };
      },
    },
    {
      class: 'Class',
      name: 'of',
      // TODO(markdittmer): This should be an expression, but that's not
      // working.
      factory: function() {
        return this.data.of;
      },
    },
  ],

  methods: [
    function initE() {
      const self = this;
      const removeColumn = this.removeColumn;
      this.setNodeName('div').addClass(this.myClass())
          // CSS grid layout for columns, consistent with number of columns and
          // their "gridTemplateColumn" configuration, if any.
          .add(this.slot(function(columns) {
            let gridTemplateColumns = '';
            for (const column of columns) {
              gridTemplateColumns += `${column.gridTemplateColumn} `;
            }
            return this.E('style')
                .add(`
              #${this.id} col-heads, #${this.id} ul.foam-u2-view-ScrollDAOView > li {
                padding: 0;
                border-bottom: 1px solid #d0d0d0;
                display: grid;
                grid-template-columns: ${gridTemplateColumns};
              }
              ul.foam-u2-view-ScrollDAOView > li {
                background-color: rgba(255, 255, 255, 0.6);
              }
            `);
          }))
          // Column headers.
          .add(this.slot(function(columns) {
            return this.E('col-heads')
                .forEach(columns, function(col) {
                  const ret = this.start('col-head')
                      .start('div').addClass('ellipsis').start('span').add(col.label).end().end()
                      .start('div').addClass('overlay').add(col.label).end();
                  if (removeColumn && col.name !== 'id') {
                    ret.start('i').addClass('material-icons')
                        .addClass(self.myClass('icon'))
                        .addClass(self.myClass('button'))
                        .add('clear')
                        .on('click', function() {
                          removeColumn(col);
                        })
                        .end();
                  }
                  return ret.end();
                });
          }))
          // Scrollable content.
          .start('div').addClass(this.myClass('scroll-container'))
          .start(this.scrollView).end();
    },
  ],
});
