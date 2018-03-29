// Copyright 2018 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

require('../grid_dao.es6.js');
require('../web_apis/api_compat_data.es6.js');
require('./expressions.es6.js');

foam.INTERFACE({
  name: 'QueryInterpreter',
  package: 'org.chromium.parse',

  documentation: 'Interface for applying semantics to parsed query fragments.',

  properties: [
    {
      class: 'FObjectProperty',
      of: 'org.chromium.parse.QueryInterpreter',
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
  name: 'DefaultQueryInterpreter',
  package: 'org.chromium.parse',
  implements: [
    'foam.mlang.Expressions',
    'org.chromium.parse.QueryInterpreter',
  ],

  documentation: `Basic interpreter that matches key/values to properties and
      interprets keywords in the natural way.`,

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
      return this.KEYWORD(kw);
    },
  ],
});

foam.CLASS({
  name: 'ReleaseApiQueryInterpreter',
  package: 'org.chromium.parse',
  implements: [
    'foam.mlang.Expressions',
    'org.chromium.mlang.GridExpressions',
    'org.chromium.mlang.ParseExpressions',
    'org.chromium.parse.QueryInterpreter',
  ],

  documentation: `Interpreter for handling release key matching and
      count-of-selected-releases. E.g.,

      count:3 => "Exactly 3 selected releases support API"

      fir59win10:0 => [prefix match alphabetical and numeric groups]
                   => "Not shipped (last 0 is false) in Firefox 59 Windows 10"

      chr60:t => [prefix match alphabetical and numeric groups]
              => "Shipped (t for true) in Chrome 60 on all known platforms"`,

  requires: [
    'org.chromium.apis.web.CompatProperty',
    'org.chromium.parse.DefaultQueryInterpreter',
  ],
  imports: [
    'selectable',
    'selected',
  ],

  properties: [
    {
      name: 'delegate',
      factory: function() {
        return this.DefaultQueryInterpreter.create();
      },
    },
  ],

  methods: [
    function interpretKeyValue(key, value, parser) {
      // When delegate is set and interpretation is null, pass to delegate for
      // interpretation.
      const ret = v => v === null ?
            (this.delegate ? this.delegate.interpretKeyValue(key, value, parser) :
             this.interpretKeyword(`${key}:${value}`, parser)) : v;

      if (key === 'count') {
        return ret(this.interpretCount(value, parser));
      }
      const columns = this.getMatchingColumns(key);
      if (columns.length === 0) return ret(null);
      const lowerValue = value.toLowerCase();
      const boolValue = lowerValue === 'true' ? true :
            (lowerValue === 'false' ? false : null);
      if (boolValue === null) return ret(null);

      if (columns.length === 1) {
        // AND(EQ(col, value)) => EQ(col, value).
        return this.EQ(columns[0], boolValue);
      } else {
        let parts = new Array(columns.length);
        for (let i = 0; i < columns.length; i++) {
          parts[i] = this.EQ(columns[i], boolValue);
        }
        return this.AND.apply(this, parts);
      }
    },
    function interpretKeyword(kw, parser) { return this.KEYWORD(kw); },
    function interpretCount(value, parser) {
      // When delegate is set and interpretation is null, pass to delegate for
      // interpretation.
      const ret = v => v === null ?
            (this.delegate ?
             this.delegate.interpretKeyValue('count', value, parser) :
             this.interpretKeyword(`count:${value}`, parser)) : v;

      const count = parseInt(value);
      if (Number.isNaN(count)) return ret(null);

      const cols = this.selected
            .filter(col => this.CompatProperty.isInstance(col));
      return this.EQ(this.ARRAY_COUNT(this.SEQ.apply(this, cols),
                                      this.TRUTHY()),
                     count);
    },
    function getMatchingColumns(key) {
      // Match [browser name][browser version][os name][os version] where
      // names are /[^0-9]+/ and versions are /[0-9.-]+/.
      const match = key.match(/^([^0-9]+)([0-9.-]+)?([^0-9]+)?([0-9.-]+)?/i);
      if (match === null) return [];

      const browserName = match[1] && match[1].toLowerCase();
      const browserVersion = match[2] && match[2].toLowerCase();
      const osName = match[3] && match[3].toLowerCase();
      const osVersion = match[4] && match[4].toLowerCase();

      const columns = this.selectable
            .filter(col => this.CompatProperty.isInstance(col));

      return columns.filter(
          c => (!browserName || c.release.browserName.toLowerCase()
                .startsWith(browserName)) &&
            (!browserVersion || c.release.browserVersion.toLowerCase()
             .startsWith(browserVersion)) &&
            (!osName || c.release.osName.toLowerCase()
             .startsWith(osName)) &&
            (!osVersion || c.release.osVersion.toLowerCase()
             .startsWith(osVersion)));
    },
  ],
});

foam.CLASS({
  name: 'QueryParser',
  package: 'org.chromium.parse',
  implements: ['foam.mlang.Expressions'],

  requires: [
    'foam.core.Property',
    'foam.core.String',
    'org.chromium.parse.DefaultQueryInterpreter',
    'org.chromium.parse.ReleaseApiQueryInterpreter',
  ],

  properties: [
    {
      class: 'Class',
      name: 'of',
      documentation: 'The class of items in the DAO being queried.',
    },
    {
      class: 'FObjectProperty',
      of: 'org.chromium.parse.QueryInterpreter',
      name: 'interpreter',
      factory: function() {
        return this.ReleaseApiQueryInterpreter.create({
          delegate: this.DefaultQueryInterpreter.create(),
        });
      },
    },
  ],

  methods: [
    function parseString(str, opt_name) {
      // Parse string; on complete parse, return result.
      const result = this.queryGrammar.getParse(str, opt_name);
      if (result && result.pos === str.length) return result.value;

      // Store partial parse, and extract keywords from remaining string.
      const partialParse = result && result.value;
      const keywords = str.substr(partialParse ? result.pos : 0)
            .replace(/[ \t\r\n]+/g, ' ').split(' ').filter(kw => !!kw);
      const keywordArgs = keywords.map(kw => this.KEYWORD(kw));

      // Edge case: no partial parse or keywords? return TRUE.
      if (!partialParse && keywords.length === 0) return this.TRUE;

      // Return conjunction of partial parse and keywords.
      const partialParseArgs =
            foam.mlang.predicate.And.isInstance(partialParse) ?
            partialParse.args : partialParse ? [partialParse] : [];
      return this.AND.apply(this, partialParseArgs.concat(keywordArgs));
    },
  ],

  grammars: [
    {
      name: 'queryGrammar',
      language: 'foam.parse.Parsers',
      withArgs: true,
      symbols: function(alt, chars, literalIC, not, plus, range, repeat0, seq,
                        seq1, str, sym) {
        return {
          START: sym('query'),
          query: seq1(1, sym('ws'), sym('or'), sym('ws')),

          or: plus(sym('and'), sym('orDelim')),
          orDelim: seq(sym('ws'), alt(literalIC('OR'), '|'), sym('ws')),

          and: plus(sym('expr'), sym('andDelim')),
          andDelim: alt(seq(sym('ws'), alt(literalIC('AND'), '&'), sym('ws')),
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
