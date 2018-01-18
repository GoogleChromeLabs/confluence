// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

foam.CLASS({
  package: 'org.chromium.parse',
  name: 'GridQueryParser',
  implements: ['foam.mlang.Expressions'],

  requires: [
    'foam.core.Property',
    'foam.core.String',
  ],

  properties: [
    {
      class: 'Class',
      name: 'of',
    },
    {
      class: 'FObjectArray',
      of: 'Property',
      name: 'strProps',
      factory: function() {
        return this.of.getAxiomsByClass(this.String).concat([this.of.ID]);
      },
    },
    {
      name: 'interpretKeyword',
      value: function(kw) {
        if (this.strProps.length === 1) {
          return this.CONTAINS_IC(this.strProps[0], kw);
        }
        return this.OR.apply(
            this, this.strProps.map(p => this.CONTAINS_IC(p, kw)));
      },
    },
    {
      name: 'interpretKeyValue',
      value: function(key, value) {
        const property = this.of.getAxiomByName(key);
        if (!property || !this.Property.isInstance(property))
          return this.interpretKeyword(`${key}:${value}`);
        return this.EQ(property, value);
      },
    },
  ],

  methods: [
    function parseString(str, opt_name) {
      console.log('parseString', str, opt_name);
      let ret;
      try {
        ret = this.queryGrammar.parseString(str, opt_name);
        console.log('=>', ret);
      } catch (error) {
        console.error('!!!', error);
      }
      return ret;
    },
  ],

  grammars: [
    {
      name: 'queryGrammar',
      language: 'foam.parse.Parsers',
      symbols: function() {
        return {
          START: sym('query'),
          query: seq1(1, sym('ws'), sym('or'), sym('ws')),

          or: plus(sym('and'), sym('orDelim')),
          orDelim: alt(seq(sym('ws'), literalIC('OR'), sym('ws')),
                       seq(sym('ws'), '|', sym('ws'))),

          and: plus(sym('expr'), sym('andDelim')),
          andDelim: alt(seq(sym('ws'), literalIC('AND'), sym('ws')),
                        not(sym('orDelim'), sym('wsRequired'))),

          expr: alt(sym('paren'),
                    sym('keyValue'),
                    sym('keyword')),

          paren: seq1(1, '(', sym('query'), ')'),

          keyValue: seq(sym('key'), ':', sym('value')),
          key: str(plus(alt(range('a', 'z'), range('A', 'Z'),
                            range('0', '9'), '.', '_'))),
          value: str(plus(alt(range('a', 'z'), range('A', 'Z'),
                              range('0', '9'), '.', '_', '#', ','))),

          keyword: str(plus(alt(range('a', 'z'), range('A', 'Z'),
                                range('0', '9'), '.', '_', '#'))),

          ws: repeat0(chars(' \t\r\n')),
          wsRequired: plus(chars(' \t\r\n')),
        };
      },
      actions: [
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
        function keyValue(keyColonValue) {
          const key = keyColonValue[0];
          const value = keyColonValue[2];

          return this.interpretKeyValue(key, value);
        },
        function keyword(kw) {
          return this.interpretKeyword(kw);
        },
      ],
    },
  ],
});
