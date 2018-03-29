
// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

describe('DefaultQueryInterpreter', () => {
  let Cls;
  let fakeParser;
  let interpreter;
  let E;
  beforeEach(() => {
    foam.CLASS({
      name: 'TestItem',
      package: 'org.chromium.parse.test',

      properties: ['testProp'],
    });
    Cls = foam.lookup('org.chromium.parse.test.TestItem');
    fakeParser = {of: Cls};
    interpreter = org.chromium.parse.DefaultQueryInterpreter.create();
    E = foam.mlang.ExpressionsSingleton.create();
  });

  it('should interpret keywords', () => {
    const result = interpreter.interpretKeyword('Hello, World!', fakeParser);
    expect(foam.util.equals(result, E.KEYWORD('Hello, World!'))).toBe(true);
  });

  it('should interpret matching property names', () => {
    const result = interpreter.interpretKeyValue('testProp', 42, fakeParser);
    expect(foam.util.equals(result, E.EQ(Cls.TEST_PROP, 42))).toBe(true);
  });

  it('should reinterpret unmatched key/values as keywords', () => {
    const result = interpreter.interpretKeyValue('abc', 'xyz', fakeParser);
    expect(foam.util.equals(result, E.KEYWORD('abc:xyz'))).toBe(true);
  });
});

describe('ReleaseApiQueryInterpreter', () => {
  let releases;
  let selectable;
  let selected;
  let Cls;
  let fakeParser;
  let interpreter;
  let E;
  beforeEach(() => {
    const pkg = org.chromium.apis.web;
    releases = [
      pkg.Release.create({
        browserName: 'Alpha',
        browserVersion: '1',
        osName: 'Yankee',
        osVersion: '1',
      }),
      pkg.Release.create({
        browserName: 'Alpha',
        browserVersion: '2',
        osName: 'Yankee',
        osVersion: '1',
      }),
      pkg.Release.create({
        browserName: 'Alpha',
        browserVersion: '2',
        osName: 'Yankee',
        osVersion: '2',
      }),

      // Selected releases (see slice(-2) below).
      pkg.Release.create({
        browserName: 'Alpha',
        browserVersion: '2',
        osName: 'Zulu',
        osVersion: '1',
      }),
      pkg.Release.create({
        browserName: 'Beta',
        browserVersion: '1',
        osName: 'Yankee',
        osVersion: '1',
      }),
    ];
    foam.CLASS({
      name: 'TestApi',
      package: 'org.chromium.parse.test',

      properties: [
        {
          class: 'String',
          name: 'id',
        },
      ].concat(releases.map(release => {
        return {
          class: 'org.chromium.apis.web.CompatProperty',
          name: release.id.replace(/_/g, '$').replace(/[^a-zA-Z0-9$]/g, '_')
              .replace(/[$]/g, ''),
          label: release.id.replace(/_/g, ' '),
          release,
        };
      })),
    });
    Cls = foam.lookup('org.chromium.parse.test.TestApi');
    selectable = Cls.getAxiomsByClass(pkg.CompatProperty);
    selected = selectable.slice(-2);
    const ctx = foam.createSubContext({selectable, selected});
    fakeParser = {of: Cls};
    interpreter = org.chromium.parse.ReleaseApiQueryInterpreter
        .create(null, ctx);
    E = foam.mlang.ExpressionsSingleton.create(null, ctx);
  });

  it('should interpret keywords', () => {
    const result = interpreter.interpretKeyword('Hello, World!', fakeParser);
    expect(foam.util.equals(result, E.KEYWORD('Hello, World!'))).toBe(true);
  });

  it('should match specific release', () => {
    const result = interpreter.interpretKeyValue('a1y1', 'true', fakeParser);
    expect(foam.util.equals(result, E.EQ(Cls.ALPHA1YANKEE1, true))).toBe(true);
  });

  it('should match case insensitively', () => {
    const result = interpreter.interpretKeyValue('A1y1', 'true', fakeParser);
    expect(foam.util.equals(result, E.EQ(Cls.ALPHA1YANKEE1, true))).toBe(true);
  });

  it('should reinterpret unknown keys as keywords', () => {
    const result = interpreter.interpretKeyValue('unknown', 'true', fakeParser);
    expect(foam.util.equals(result, E.KEYWORD('unknown:true'))).toBe(true);
  });

  it('should interpret counts in terms of selected browsers', () => {
    const result = interpreter.interpretKeyValue('count', '1', fakeParser);
    expect(foam.util.equals(
        result,
        E.EQ(E.ARRAY_COUNT(E.SEQ(Cls.ALPHA2ZULU1, Cls.BETA1YANKEE1),
                           E.TRUTHY()), 1)))
        .toBe(true);
  });

  it('should interpret non-numeric counts as keywords', () => {
    const result = interpreter.interpretKeyValue('count', 'NaN', fakeParser);
    expect(foam.util.equals(result,E.KEYWORD('count:NaN'))).toBe(true);
  });

  it('should interpret partial matches', () => {
    let result;
    result = interpreter.interpretKeyValue('alpha', 'true', fakeParser);
    expect(foam.util.equals(
        result, E.AND(
            E.EQ(Cls.ALPHA1YANKEE1, true),
            E.EQ(Cls.ALPHA2YANKEE1, true),
            E.EQ(Cls.ALPHA2YANKEE2, true),
            E.EQ(Cls.ALPHA2ZULU1, true)))).toBe(true);
    result = interpreter.interpretKeyValue('a2', 'true', fakeParser);
    expect(foam.util.equals(
        result, E.AND(
            E.EQ(Cls.ALPHA2YANKEE1, true),
            E.EQ(Cls.ALPHA2YANKEE2, true),
            E.EQ(Cls.ALPHA2ZULU1, true)))).toBe(true);
    result = interpreter.interpretKeyValue('beta', 'true', fakeParser);
    expect(foam.util.equals(result, E.EQ(Cls.BETA1YANKEE1, true))).toBe(true);
  });

  it('should support various truthinesses', () => {
    [
      'true',
      'True',
      'TRUE',
      'TrUe',
    ].forEach(value => {
      const result = interpreter.interpretKeyValue('a1y1', value, fakeParser);
      expect(foam.util.equals(result, E.EQ(Cls.ALPHA1YANKEE1, true)))
          .toBe(true);
    });
  });

  it('should support various falsinesses', () => {
    [
      'false',
      'False',
      'FALSE',
      'FaLsE',
    ].forEach(value => {
      const result = interpreter.interpretKeyValue('a1y1', value, fakeParser);
      expect(foam.util.equals(result, E.EQ(Cls.ALPHA1YANKEE1, false)))
          .toBe(true);
    });
  });
});

describe('QueryParser', () => {
  let parser;
  let E;
  let KEY_VALUE;
  beforeEach(() => {
    foam.CLASS({
      name: 'KeyValue',
      package: 'org.chromium.parse.test',

      properties: [
        {class: 'String', name: 'key'},
        {class: 'String', name: 'value'},
      ],
    });
    foam.CLASS({
      name: 'FakeQueryInterpreter',
      package: 'org.chromium.parse.test',
      implements: [
        'foam.mlang.Expressions',
        'org.chromium.parse.QueryInterpreter',
      ],

      requires: ['org.chromium.parse.test.KeyValue'],

      methods: [
        function interpretKeyValue(key, value) {
          return this.KeyValue.create({key, value});
        },
        function interpretKeyword(keyword) {
          return this.KEYWORD(keyword);
        },
      ],
    });
    parser = org.chromium.parse.QueryParser.create({
      interpreter: org.chromium.parse.test.FakeQueryInterpreter.create(),
    });
    E = foam.mlang.ExpressionsSingleton.create();
    KEY_VALUE =
        (key, value) => org.chromium.parse.test.KeyValue.create({key, value});
  });

  it('should strip whitspace', () => {
    const str = '  foo\tbar\n  ';
    const result = parser.parseString(str);
    expect(foam.util.equals(result, E.AND(E.KEYWORD('foo'),
                                          E.KEYWORD('bar')))).toBe(true);
  });

  it('should recognize key/values with no spaces', () => {
    const str = 'foo:bar';
    const result = parser.parseString(str);
    expect(foam.util.equals(result, KEY_VALUE('foo', 'bar'))).toBe(true);
  });

  it('combine over spaces after partial parse', () => {
    // Parse succeeds up to AND(KEY_VALUE(foo, bar), KEYWORD(baz)), then halts.
    // ": \t quz  " should be interpreted as whitespace-separated keywords.
    const str = 'foo:bar baz: \t quz  ';
    const result = parser.parseString(str);
    expect(foam.util.equals(
        result,
        E.AND(KEY_VALUE('foo', 'bar'), E.KEYWORD('baz'), E.KEYWORD(':'),
              E.KEYWORD('quz'))))
        .toBe(true);
  });

  it('should recognize and/or', () => {
    let str;
    let result;

    str = 'a and b or c';
    result = parser.parseString(str);
    expect(foam.util.equals(
        result,
        E.OR(E.AND(E.KEYWORD('a'), E.KEYWORD('b')), E.KEYWORD('c'))))
        .toBe(true);

    str = 'a & b or c';
    result = parser.parseString(str);
    expect(foam.util.equals(
        result,
        E.OR(E.AND(E.KEYWORD('a'), E.KEYWORD('b')), E.KEYWORD('c'))))
        .toBe(true);

    str = 'a and b | c';
    result = parser.parseString(str);
    expect(foam.util.equals(
        result,
        E.OR(E.AND(E.KEYWORD('a'), E.KEYWORD('b')), E.KEYWORD('c'))))
        .toBe(true);

    str = 'a & b | c';
    result = parser.parseString(str);
    expect(foam.util.equals(
        result,
        E.OR(E.AND(E.KEYWORD('a'), E.KEYWORD('b')), E.KEYWORD('c'))))
        .toBe(true);

    str = 'a and ( b or c)';
    result = parser.parseString(str);
    expect(foam.util.equals(
        result,
        E.AND(E.KEYWORD('a'), E.OR(E.KEYWORD('b'), E.KEYWORD('c')))))
        .toBe(true);
  });
});
