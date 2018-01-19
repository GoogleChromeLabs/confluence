// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

foam.INTERFACE({
  package: 'org.chromium.parse',
  name: 'GridQueryInterpreter',

  documentation: `A component responsible for interpreting the semantics of
      low-level GridDAO query parts.`,

  properties: [
    {
      class: 'FObjectProperty',
      of: 'org.chromium.parse.GridQueryInterpreter',
      documentation: 'Support proxying/delegating by default.',
      name: 'delegate',
      value: null,
    },
  ],

  methods: [
    {
      name: 'interpretKeyValue',
      returns: 'foam.mlang.predicate.AbstractPredicate',
      args: [
        {
          typeName: 'String',
          documentation: 'The string key in a "key:value" query fragment.',
          name: 'key',
        },
        {
          typeName: 'String',
          documentation: 'The string value in a "key:value" query fragment.',
          name: 'value',
        },
      ],
    },
    {
      name: 'interpretKeyword',
      returns: 'foam.mlang.predicate.AbstractPredicate',
      args: [
        {
          typeName: 'String',
          documentation: 'The keyword in a keyword query fragment.',
          name: 'kw',
        },
      ],
    },
  ],
});

foam.CLASS({
  package: 'org.chromium.parse',
  name: 'DefaultGridQueryInterpreter',
  implements: [
    'foam.mlang.Expressions',
    'org.chromium.parse.GridQueryInterpreter',
  ],

  requires: ['foam.core.Property'],

  methods: [
    function interpretKeyValue(key, value, parser) {
      const property = parser.of.getAxiomByName(key);
      if (!property || !this.Property.isInstance(property)) {
        return this.delegate ?
            this.delegate.interpretKeyValue(key, value, parser) :
            this.interpretKeyword(`${key}:${value}`, parser);
      }
      return this.EQ(property, value);
    },
    function interpretKeyword(kw, parser) {
      if (parser.strProps.length === 1) {
        return this.CONTAINS_IC(parser.strProps[0], kw);
      }
      return this.OR.apply(
        this, parser.strProps.map(p => this.CONTAINS_IC(p, kw)));
    },
  ],
});

foam.CLASS({
  package: 'org.chromium.parse',
  name: 'ReleaseApiGridQueryInterpreter',
  implements: [
    'org.chromium.mlang.GridExpressions',
    'org.chromium.parse.GridQueryInterpreter',
  ],
  requires: ['org.chromium.parse.DefaultGridQueryInterpreter'],

  imports: [
    'gridDAO',
    'selectedCols',
  ],

  properties: [
    {
      name: 'delegate',
      factory: function() {
        return this.DefaultGridQueryInterpreter.create();
      },
    },
  ],

  methods: [
    function interpretKeyValue(key, value, parser) {
      const ret = this.delegate ?
          (v => v === null ?
              this.delegate.interpretKeyValue(key, value, parser) : v) :
          (v => v === null ?
              this.interpretKeyword(`${key}:${value}`, parser) : v);

      if (key === 'count') {
        return ret(this.interpretCount(value, parser));
      }
      const releases = this.getMatchingReleases(key);
      if (releases.length === 0) return ret(null);
      const boolValue = /^[t1-9]/i.test(value) ? true :
              /^(f.*|0)$/i.test(value) ? false : null;
      if (boolValue === null) return ret(null);

      if (releases.count === 1) {
        return this.EQ(this.gridDAO.extractCols(releases[0]), boolValue);
      } else {
        let bools = new Array(releases.length);
        for (let i = 0; i < releases.length; i++) {
          bools[i] = boolValue;
        }
        return this.EQ(this.gridDAO.extractCols.apply(this.gridDAO, releases),
                       bools);
      }
    },
    function interpretKeyword(kw, parser) {
        if (parser.strProps.length === 1) {
          return this.CONTAINS_IC(parser.strProps[0], kw);
        }
        return this.OR.apply(
            parser, parser.strProps.map(p => this.CONTAINS_IC(p, kw)));
    },
    function interpretCount(value, parser) {
      const ret = this.delegate ?
          (v => v === null ?
              this.delegate.interpretKeyValue('count', value, parser) : v) :
          (v => v === null ?
              this.interpretKeyword(`count:${value}`, parser) : v);

      const counts = value.split(',').map(countStr => parseInt(countStr));
      if (counts.length === 0) return ret(null);
      for (const count of counts) {
        if (Number.isNaN(count)) return ret(null);
      }

      if (counts.length === 1) {
        return this.EQ(this.ARRAY_COUNT(
            this.gridDAO.extractCols.apply(this.gridDAO, this.selectedCols),
            this.TRUTHY()), counts[0]);
      } else {
        return this.IN(this.ARRAY_COUNT(
            this.gridDAO.extractCols.apply(this.gridDAO, this.selectedCols),
            this.TRUTHY()), counts);
      }
    },
    function getMatchingReleases(key) {
      const match = key.match(/^([^0-9]+)([0-9.-]+)?([^0-9]+)?([0-9.-]+)?/i);
      if (match === null) return [];

      const browserName = match[1] && match[1].toLowerCase();
      const browserVersion = match[2] && match[2].toLowerCase();
      const osName = match[3] && match[3].toLowerCase();
      const osVersion = match[4] && match[4].toLowerCase();

      return this.gridDAO.cols.filter(
        r => (!browserName ||
              r.browserName.toLowerCase().startsWith(browserName)) &&
          (!browserVersion ||
           r.browserVersion.toLowerCase().startsWith(browserVersion)) &&
          (!osName ||
           r.osName.toLowerCase().startsWith(osName)) &&
          (!osVersion ||
           r.osVersion.toLowerCase().startsWith(osVersion)));
    },
  ],
});

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
      documentation: 'The class of items in the DAO being queried.',
    },
    {
      class: 'FObjectArray',
      of: 'Property',
      name: 'strProps',
      documentation: `The string properies of "of" that accept keyword
          queries.`,
      factory: function() {
        return this.of.getAxiomsByClass(this.String).concat([this.of.ID]);
      },
    },
    {
      class: 'FObjectProperty',
      of: 'org.chromium.parse.GridQueryInterpreter',
      name: 'interpreter',
      factory: function() {
        return this.DefaultGridQueryInterpreter.create();
      },
    },
  ],

  methods: [
    function parseString(str, opt_name) {
      return this.queryGrammar.parseString(str, opt_name);
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

          return this.interpreter.interpretKeyValue(key, value, this);
        },
        function keyword(kw) {
          return this.interpreter.interpretKeyword(kw, this);
        },
      ],
    },
  ],
});
