
// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

describe('QueryParser', () => {
  let releases;
  let Cls;
  let selectable;
  let selected;
  let parser;
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
    parser = org.chromium.parse.QueryParser.create(null, ctx);
    E = foam.mlang.ExpressionsSingleton.create();
  });

  it('should parse keywords', () => {
    const parse = parser.parseString('frobinator');
    expect(parse.rest).toBe('');
    expect(foam.util.equals(parse.result, E.KEYWORD('frobinator'))).toBe(true);
  });

  it('should match specific release', () => {
    const parse = parser.parseString('in:a1y1');
    expect(parse.rest).toBe('');
    expect(foam.util.equals(parse.result, E.EQ(Cls.ALPHA1YANKEE1, true)))
        .toBe(true);
  });

  it('should match case insensitively', () => {
    const parse = parser.parseString('iN:A1y1');
    expect(parse.rest).toBe('');
    expect(foam.util.equals(parse.result, E.EQ(Cls.ALPHA1YANKEE1, true)))
        .toBe(true);
  });

  it('should emit partial parse at point of unexpected input', () => {
    const parse = parser.parseString('alpha beta ~gamma');
    expect(parse.rest).toBe('~gamma');
    expect(foam.util.equals(parse.result,
                            E.AND(E.KEYWORD('alpha'), E.KEYWORD('beta'))));
  });

  it('should interpret counts in terms of selected browsers', () => {
    const parse = parser.parseString('count:1');
    expect(parse.rest).toBe('');
    expect(foam.util.equals(
        parse.result,
        E.EQ(E.ARRAY_COUNT(E.SEQ(Cls.ALPHA2ZULU1, Cls.BETA1YANKEE1),
                           E.TRUTHY()), 1)))
        .toBe(true);
  });

  it('should not produce EQ(ARRAY_COUNT(...)...) with "count:NaN"', () => {
    const result = parser.parseString('count:NaN').result;
    expect(foam.mlang.predicate.Eq.isInstance(result) &&
           org.chromium.mlang.ArrayCount.isInstance(result.arg1))
        .toBe(false);
  });

  it('should produce partial matches on releases', () => {
    let parse;

    parse = parser.parseString('in:alpha');
    expect(parse.rest).toBe('');
    expect(foam.util.equals(
        parse.result, E.AND(
            E.EQ(Cls.ALPHA1YANKEE1, true),
            E.EQ(Cls.ALPHA2YANKEE1, true),
            E.EQ(Cls.ALPHA2YANKEE2, true),
            E.EQ(Cls.ALPHA2ZULU1, true)))).toBe(true);
    parse = parser.parseString('in:a2');
    expect(parse.rest).toBe('');
    expect(foam.util.equals(
        parse.result, E.AND(
            E.EQ(Cls.ALPHA2YANKEE1, true),
            E.EQ(Cls.ALPHA2YANKEE2, true),
            E.EQ(Cls.ALPHA2ZULU1, true)))).toBe(true);
    parse = parser.parseString('in:beta');
    expect(parse.rest).toBe('');
    expect(foam.util.equals(parse.result, E.EQ(Cls.BETA1YANKEE1, true)))
        .toBe(true);

    parse = parser.parseString('notin:alpha');
    expect(parse.rest).toBe('');
    expect(foam.util.equals(
        parse.result, E.AND(
            E.EQ(Cls.ALPHA1YANKEE1, false),
            E.EQ(Cls.ALPHA2YANKEE1, false),
            E.EQ(Cls.ALPHA2YANKEE2, false),
            E.EQ(Cls.ALPHA2ZULU1, false)))).toBe(true);
    parse = parser.parseString('notin:a2');
    expect(parse.rest).toBe('');
    expect(foam.util.equals(
        parse.result, E.AND(
            E.EQ(Cls.ALPHA2YANKEE1, false),
            E.EQ(Cls.ALPHA2YANKEE2, false),
            E.EQ(Cls.ALPHA2ZULU1, false)))).toBe(true);
    parse = parser.parseString('notin:beta');
    expect(parse.rest).toBe('');
    expect(foam.util.equals(parse.result, E.EQ(Cls.BETA1YANKEE1, false)))
        .toBe(true);
  });

  it('should strip whitspace', () => {
    const str = '  foo\tbar\n  ';
    const parse = parser.parseString(str);
    expect(parse.rest).toBe('');
    expect(foam.util.equals(parse.result, E.AND(E.KEYWORD('foo'),
                                                E.KEYWORD('bar')))).toBe(true);
  });

  it('should throw on semantics interpretation error', () => {
    expect(() => parser.parseString('in:notabrowser')).toThrow();
  });

  it('should recognize and/or', () => {
    let str;
    let parse;

    str = 'a and b or c';
    parse = parser.parseString(str);
    expect(parse.rest).toBe('');
    expect(foam.util.equals(
        parse.result,
        E.OR(E.AND(E.KEYWORD('a'), E.KEYWORD('b')), E.KEYWORD('c'))))
        .toBe(true);

    str = 'a & b or c';
    parse = parser.parseString(str);
    expect(parse.rest).toBe('');
    expect(foam.util.equals(
        parse.result,
        E.OR(E.AND(E.KEYWORD('a'), E.KEYWORD('b')), E.KEYWORD('c'))))
        .toBe(true);

    str = 'a and b | c';
    parse = parser.parseString(str);
    expect(parse.rest).toBe('');
    expect(foam.util.equals(
        parse.result,
        E.OR(E.AND(E.KEYWORD('a'), E.KEYWORD('b')), E.KEYWORD('c'))))
        .toBe(true);

    str = 'a & b | c';
    parse = parser.parseString(str);
    expect(parse.rest).toBe('');
    expect(foam.util.equals(
        parse.result,
        E.OR(E.AND(E.KEYWORD('a'), E.KEYWORD('b')), E.KEYWORD('c'))))
        .toBe(true);

    str = 'a and ( b or c)';
    parse = parser.parseString(str);
    expect(parse.rest).toBe('');
    expect(foam.util.equals(
        parse.result,
        E.AND(E.KEYWORD('a'), E.OR(E.KEYWORD('b'), E.KEYWORD('c')))))
        .toBe(true);
  });

  it('should generate appropriate terse release IDs', () => {
    const releaseIDs = [
      'Alpha_1.0_NewOS_3.1.11',
      'Beta_54.1.443252_OldOS_15.0',
      'Alpha_2.2.1181_OldOS_12.1',
    ];

    const terseIDs = releaseIDs.map(org.chromium.parse.util.getTerseReleaseId);

    expect(terseIDs).toEqual([
      'alp1.0new3.1',
      'bet54.1old15.0',
      'alp2.2old12.1',
    ]);
  });
});
