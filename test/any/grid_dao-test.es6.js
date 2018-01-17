// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';


describe('GridDAO', () => {
  it('should use FOAM object equality for colIndexOf', () => {
    foam.CLASS({
      package: 'org.chromium.apis.web.test',
      name: 'RGBA',

      properties: [
        {
          class: 'Float',
          name: 'r',
        },
        {
          class: 'Float',
          name: 'g',
        },
        {
          class: 'Float',
          name: 'b',
        },
        {
          class: 'Float',
          name: 'a',
          value: 1.0,
        },
      ],
    });

    foam.CLASS({
      package: 'org.chromium.apis.web.test',
      name: 'Color',

      properties: [
        {
          class: 'String',
          name: 'name',
        },
        {
          class: 'FObjectProperty',
          of: 'org.chromium.apis.web.test.RGBA',
          name: 'rgba',
        },
      ],
    });

    const RGBA = org.chromium.apis.web.test.RGBA;
    const Color = org.chromium.apis.web.test.Color;

    const dao = org.chromium.apis.web.GridDAO.create({
      cols: [
        Color.create({name: 'red', rgba: RGBA.create({r: 1.0})}),
        Color.create({name: 'green', rgba: RGBA.create({g: 1.0})}),
        Color.create({name: 'blue', rgba: RGBA.create({b: 1.0})}),
      ],
    });

    expect(dao.colIndexOf(Color.create())).toBe(-1);
    expect(dao.colIndexOf(null)).toBe(-1);
    expect(dao.colIndexOf(undefined)).toBe(-1);
    expect(dao.colIndexOf(foam.core.FObject.create())).toBe(-1);
    expect(dao.colIndexOf(-1)).toBe(-1);
    expect(dao.colIndexOf(true)).toBe(-1);
    expect(dao.colIndexOf(false)).toBe(-1);
    expect(dao.colIndexOf(Infinity)).toBe(-1);

    expect(dao.colIndexOf(Color.create({
      name: 'red',
      rgba: RGBA.create({r: 1.0}),
    }))).toBe(0);
    expect(dao.colIndexOf(Color.create({
      name: 'green',
      rgba: RGBA.create({g: 1.0}),
    }))).toBe(1);
    expect(dao.colIndexOf(Color.create({
      name: 'blue',
      rgba: RGBA.create({b: 1.0}),
    }))).toBe(2);
  });
});
