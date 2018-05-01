// Copyright 2018 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

require('../dao/grid_dao.es6.js');
require('../web_apis/api_compat_data.es6.js');
require('./expressions.es6.js');

foam.CLASS({
  name: 'QueryParser',
  package: 'org.chromium.parse',
  implements: ['foam.mlang.Expressions'],

  requires: ['org.chromium.apis.web.CompatProperty'],
  imports: [
    'selectable',
    'selected',
  ],

  properties: [
    {
      class: 'Class',
      name: 'of',
      documentation: 'The class of items in the DAO being queried.',
    },
  ],

  methods: [
    function parseString(str, opt_name) {
      const parse = this.queryGrammar.getParse(str, opt_name);
      if (!parse) throw new Error(`QueryParser failed to parse: "${str}"`);
      return {result: parse.value, rest: str.substr(parse.pos)};
    },
    function matchReleaseColumns_(values) {
      const browserName = values[0].toLowerCase();
      const browserVersion = values[1] && values[1].toLowerCase();
      const osName = values[2] && values[2].toLowerCase();
      const osVersion = values[3] && values[3].toLowerCase();

      const columns = this.selectable
            .filter(col => this.CompatProperty.isInstance(col));

      return columns.filter(
          c => c.release.browserName.toLowerCase().startsWith(browserName) &&
            (!browserVersion || c.release.browserVersion.toLowerCase()
             .startsWith(browserVersion)) &&
            (!osName || c.release.osName.toLowerCase()
             .startsWith(osName)) &&
            (!osVersion || c.release.osVersion.toLowerCase()
             .startsWith(osVersion)));
    },
  ],

  grammars: [
    {
      name: 'queryGrammar',
      language: 'foam.parse.Parsers',
      withArgs: true,
      symbols: function(alt, chars, literalIC, not, optional, plus, range,
                        repeat0, seq, seq1, str, sym) {
        return {
          START: sym('query'),
          query: optional(seq1(1, sym('ws'), sym('or'), sym('ws'))),

          or: plus(sym('and'), sym('orDelim')),
          orDelim: seq(sym('ws'), alt(literalIC('OR'), '|'), sym('ws')),

          and: plus(sym('expr'), sym('andDelim')),
          andDelim: alt(seq(sym('ws'), alt(literalIC('AND'), '&'), sym('ws')),
                        not(sym('orDelim'), sym('wsRequired'))),

          expr: alt(sym('paren'),
                    sym('countExpr'),
                    sym('inExpr'),
                    sym('notinExpr'),
                    sym('keyword')),

          paren: seq1(1, '(', sym('query'), ')'),

          countExpr: seq(literalIC('COUNT:'), str(plus(range('0', '9')))),
          inExpr: seq(literalIC('IN:'), sym('releaseValue')),
          notinExpr: seq(literalIC('NOTIN:'), sym('releaseValue')),

          releaseValue: seq(sym('name'), optional(sym('version')),
                            optional(sym('name')), optional(sym('version'))),
          name: str(plus(alt(range('a', 'z'), range('A', 'Z')))),
          version: str(plus(alt(range('0', '9'), '.'))),

          keyword: str(plus(alt(range('a', 'z'), range('A', 'Z'),
                                range('0', '9'), '.', '_', '#'))),

          ws: repeat0(chars(' \t\r\n')),
          wsRequired: plus(chars(' \t\r\n')),
        };
      },
      actions: [
        function query(optionalOr) {
          return optionalOr || this.TRUE;
        },
        function or(exprs) {
          if (exprs.length === 0) return this.TRUE;
          if (exprs.length === 1) return exprs[0];
          return this.OR.apply(this, exprs);
        },
        function and(exprs) {
          if (exprs.length === 0) return this.TRUE;
          if (exprs.length === 1) return exprs[0];
          return this.AND.apply(this, exprs);
        },
        function countExpr(countValue) {
          const value = parseInt(countValue[1]);
          const cols = this.selected
                .filter(col => this.CompatProperty.isInstance(col));
          return this.EQ(this.ARRAY_COUNT(this.SEQ.apply(this, cols),
                                          this.TRUTHY()),
                         value);
        },
        function inExpr(inAndValues) {
          const values = inAndValues[1];
          const cols = this.matchReleaseColumns_(values);

          if (cols.length === 0) {
            throw new Error(`No releases match fragment ${
              values.filter(v => !!v).join('')
            }`);
          }

          return cols.length === 1 ? this.EQ(cols[0], true) :
              this.AND.apply(
                  this, cols.map(col => this.EQ(col, true)));
        },
        function notinExpr(notinAndValues) {
          const values = notinAndValues[1];
          const cols = this.matchReleaseColumns_(values);

          if (cols.length === 0) {
            throw new Error(`No releases match fragment ${
              values.filter(v => !!v).join('')
            }`);
          }

          return cols.length === 1 ? this.EQ(cols[0], false) :
              this.AND.apply(
                  this, cols.map(col => this.EQ(col, false)));
        },
        function keyword(kw) {
          return this.KEYWORD(kw);
        },
      ],
    },
  ],
});
