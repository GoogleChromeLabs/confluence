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

  it('should project appropriately on proper use of projectCols()', done => {
    foam.CLASS({
      package: 'org.chromium.apis.web.test',
      name: 'Col',

      properties: [
        {
          class: 'String',
          name: 'id',
        },
      ],
    });
    const Col = org.chromium.apis.web.test.Col;
    const dao = org.chromium.apis.web.GridDAO.create({
      cols: [
        Col.create({id: 'A'}),
        Col.create({id: 'B'}),
        Col.create({id: 'C'}),
        Col.create({id: 'D'}),
      ],
    });
    const E = foam.mlang.ExpressionsSingleton.create();
    const GridRow = org.chromium.apis.web.GridRow;
    Promise.all([
      dao.put(GridRow.create({id: 0, data: [1, 0, 0, 0]})),
      dao.put(GridRow.create({id: 1, data: [0, 1, 0, 0]})),
      dao.put(GridRow.create({id: 2, data: [0, 0, 1, 0]})),
      dao.put(GridRow.create({id: 3, data: [0, 0, 0, 1]})),
    ]).then(() => {
      return dao.orderBy(GridRow.ID).select(E.MAP(
          dao.projectCols(dao.cols[3], dao.cols[2], dao.cols[1], dao.cols[0])));
    }).then(mapSink => {
      const array = mapSink.delegate.array;
      expect(array[0].id).toBe(0);
      expect(array[1].id).toBe(1);
      expect(array[2].id).toBe(2);
      expect(array[3].id).toBe(3);
      expect(array[0].data).toEqual([0, 0, 0, 1]);
      expect(array[1].data).toEqual([0, 0, 1, 0]);
      expect(array[2].data).toEqual([0, 1, 0, 0]);
      expect(array[3].data).toEqual([1, 0, 0, 0]);
      done();
    }).catch(done.fail);
  });

  it('should project appropriately over a subset of cols using projectCols()', done => {
    foam.CLASS({
      package: 'org.chromium.apis.web.test',
      name: 'Col',

      properties: [
        {
          class: 'String',
          name: 'id',
        },
      ],
    });
    const Col = org.chromium.apis.web.test.Col;
    const dao = org.chromium.apis.web.GridDAO.create({
      cols: [
        Col.create({id: 'A'}),
        Col.create({id: 'B'}),
        Col.create({id: 'C'}),
        Col.create({id: 'D'}),
      ],
    });
    const E = foam.mlang.ExpressionsSingleton.create();
    const GridRow = org.chromium.apis.web.GridRow;
    Promise.all([
      dao.put(GridRow.create({id: 0, data: [1, 0, 0, 0]})),
      dao.put(GridRow.create({id: 1, data: [0, 1, 0, 0]})),
      dao.put(GridRow.create({id: 2, data: [0, 0, 1, 0]})),
      dao.put(GridRow.create({id: 3, data: [0, 0, 0, 1]})),
    ]).then(() => {
      return dao.orderBy(GridRow.ID).select(E.MAP(
          dao.projectCols(dao.cols[3], dao.cols[1])));
    }).then(mapSink => {
      const array = mapSink.delegate.array;
      expect(array[0].id).toBe(0);
      expect(array[1].id).toBe(1);
      expect(array[2].id).toBe(2);
      expect(array[3].id).toBe(3);
      expect(array[0].data).toEqual([0, 0]);
      expect(array[1].data).toEqual([0, 1]);
      expect(array[2].data).toEqual([0, 0]);
      expect(array[3].data).toEqual([1, 0]);
      done();
    }).catch(done.fail);
  });

  it('should warn and ignore invalid projection columns', done => {
    const warn = foam.__context__.warn;
    let warnCount = 0;
    const ctx = foam.createSubContext({
      warn: function() {
        warnCount++;
        return warn.apply(this, arguments);
      },
    });
    foam.CLASS({
      package: 'org.chromium.apis.web.test',
      name: 'Col',

      properties: [
        {
          class: 'String',
          name: 'id',
        },
      ],
    }, ctx);
    const Col = org.chromium.apis.web.test.Col;
    const dao = org.chromium.apis.web.GridDAO.create({
      cols: [
        Col.create({id: 'A'}),
      ],
    }, ctx);
    const E = foam.mlang.ExpressionsSingleton.create(null, ctx);
    const GridRow = org.chromium.apis.web.GridRow;
    Promise.all([
      dao.put(GridRow.create({id: 0, data: [0]}, ctx)),
      dao.put(GridRow.create({id: 1, data: [1]}, ctx)),
      dao.put(GridRow.create({id: 2, data: [2]}, ctx)),
      dao.put(GridRow.create({id: 3, data: [3]}, ctx)),
    ]).then(() => {
      warnCount = 0;
      const sink = E.MAP(dao.projectCols(
          Col.create({id: 'B'}, ctx), dao.cols[0], null, undefined, -1));
      expect(warnCount).toBe(4);
      return dao.orderBy(GridRow.ID).select(sink);
    }).then(mapSink => {
      const array = mapSink.delegate.array;
      expect(array[0].id).toBe(0);
      expect(array[1].id).toBe(1);
      expect(array[2].id).toBe(2);
      expect(array[3].id).toBe(3);
      expect(array[0].data).toEqual([0]);
      expect(array[1].data).toEqual([1]);
      expect(array[2].data).toEqual([2]);
      expect(array[3].data).toEqual([3]);
      done();
    }).catch(done.fail);
  });
});
