// Copyright 2018 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

require('../parse/parser_interpreter.es6.js');

foam.CLASS({
  package: 'org.chromium.apis.web',
  name: 'SearchView',
  extends: 'foam.u2.View',

  documentation: `A search box decorated with a magnifying glass label and (when
      search query is non-empty) clear button. Query strings are parsed and the
      result (if any) is bound to the imported predicate.`,

  requires: [
    'org.chromium.parse.QueryParser',
  ],
  imports: [
    'error',
    'predicate',
    'query? as importedQuery',
  ],

  css: `
    ^ {
      display: flex;
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
      background-color: rgba(0, 0, 0, 0.2);
      cursor: pointer;
    }

    ^ input {
      color: inherit;
      background-color: inherit;
      font-family: inherit;
      font-size: inherit;
      flex-grow: 1;
      border: none;
      outline: none;
      display: inline-block;
      line-height: 2rem;
      padding: 0.5rem 0;
    }

    ^ .hide {
      display: none;
    }
  `,

  properties: [
    {
      class: 'Class',
      name: 'of',
      documentation: 'The class of objects being searched for.',
      required: true,
    },
    {
      name: 'queryParser',
      documentation: `The parser used to generate a filter predicate from the
          query string.`,
      factory: function() {
        return this.QueryParser.create({of: this.of});
      },
    },
    {
      class: 'foam.u2.ViewSpec',
      name: 'viewSpec',
      documentation: 'Spec for constructing search input view.',
      value: {
        class: 'foam.u2.tag.Input',
        onKey: true,
      },
    },
    {
      name: 'view',
    },
  ],

  actions: [
    {
      name: 'clear',
      documentation: 'Clear the current query string.',
      keyboardShortcuts: [27], // Escape.
      code: function() { this.view.data = ''; },
    },
  ],

  methods: [
    function initE() {
      this.addClass(this.myClass())
          .start('i').addClass('material-icons').addClass(this.myClass('icon'))
          .add('search')
          .end()
          .tag(this.viewSpec, {}, this.view$)
          .start('i').addClass('material-icons')
          .addClass(this.myClass('icon'))
          .addClass(this.myClass('button'))
          .addClass(this.slot(data => data ? '' : 'hide',
                              this.view$.dot('data')))
          .add('close')
          .on('click', () => this.clear());

      // Optionally bind context's query string to UI value.
      if (!foam.Undefined.isInstance(this.importedQuery)) {
        this.importedQuery$.linkTo(this.view$.dot('data'));
      }

      this.view.data$.mapTo(
          this.predicate$,
          queryStr => {
            let ret = this.predicate;
            try {
              const parse = this.queryParser.parseString(queryStr || '');
              // TODO(markdittmer): Surface report of partial parse when
              // parse.rest is not "".
              ret = parse.result;
            } catch (e) {
              // TODO(markdittmer): Surface parse failure in UI.
              this.error('Query parser error: ', e);
            }
            return ret;
          });
      this.SUPER();
    },
  ],
});
