// Copyright 2018 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

global.describeRawTableCellFormatterTests = (clsId, propName, objsFactory) => {
  function setUp() {
    let cls = foam.lookup(clsId);
    let prop = cls.getAxiomByName(propName);
    let objs = objsFactory(cls, prop);
    if (!Array.isArray(objs)) objs = [objs];

    return {cls, prop, objs};
  }

  describe(`rawTableCellFormatter: ${clsId}.${propName}`, () => {
    it('should handle undefined, null, 0, "", false', () => {
      const data = setUp();
      for (const value of [undefined, null, 0, "", false]) {
        for (const obj of data.objs) {
          expect(foam.String.isInstance(
              data.prop.rawTableCellFormatter(value, obj, data.prop)))
              .toBe(true);
        }
      }
    });

    it('should not trust value.toString()', () => {
      const data = setUp();
      const EVIL = '__EVIL_STRING_INJECTION__';
      for (const obj of data.objs) {
        const htmlStr = data.prop.rawTableCellFormatter({
          toString: () => EVIL,
        }, obj, data.prop);
        expect(htmlStr.indexOf(EVIL)).toBe(-1);
      }
    });

    // TODO(markdittmer): Find a strict HTML parser to check soundness of
    // return string.
  });
};
