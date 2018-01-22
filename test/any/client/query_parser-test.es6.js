// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

describe('QueryParser', () => {
  let gridDAO;
  let cols;
  let selectedCols;
  let rows;
  let parser;
  let T;
  let F;

  beforeEach(() => {
    cols = foam.json.parse([
      {
        browserName: "BrowserA",
        browserVersion: "1.0",
        osName: "FooOS",
        osVersion: "1.0",
        releaseDate: "2017-07-25T00:00:00.000Z",
      },
      {
        browserName: "BrowserA",
        browserVersion: "2.0",
        osName: "BarOS",
        osVersion: "1.0",
        releaseDate: "2017-07-25T00:00:00.000Z",
      },
      {
        browserName: "BrowserB",
        browserVersion: "10.0",
        osName: "BarOS",
        osVersion: "1.0",
        releaseDate: "2017-08-08T00:00:00.000Z",
      },
      {

        browserName: "BrowserB",
        browserVersion: "11.0",
        osName: "BarOS",
        osVersion: "2.0",
        releaseDate: "2017-08-08T00:00:00.000Z",
      },
      {

        browserName: "BrowserB",
        browserVersion: "12.0",
        osName: "BarOS",
        osVersion: "2.0",
        releaseDate: "2017-09-05T00:00:00.000Z",
      },
    ], org.chromium.apis.web.Release);
    selectedCols = [
      cols[0], cols[1], cols[cols.length -1],
    ];
    gridDAO = org.chromium.apis.web.GridDAO.create({
      cols,
      // TODO(markdittmer): Overridding this should be unnecessary. Remove this
      // once https://github.com/foam-framework/foam2/issues/980 is fixed.
      delegate: foam.dao.ArrayDAO.create({
        of: org.chromium.apis.web.GridRow,
      }),
    });

    T = true;
    F = false;
    rows = foam.json.parse([
      {id: "Alpha#a", data: [F, T, T, T, T]},
      {id: "Alpha#b", data: [T, F, T, T, T]},
      {id: "Alpha#c", data: [T, T, F, T, T]},
      {id: "Beta#x",  data: [T, T, T, F, T]},
      {id: "Beta#y",  data: [T, T, T, T, F]},
      {id: "Gamma#q", data: [T, T, F, F, F]},
      {id: "Gamma#r", data: [F, T, T, F, F]},
      {id: "Gamma#s", data: [F, F, T, T, F]},
      {id: "Gamma#t", data: [F, F, F, T, T]},
    ], org.chromium.apis.web.GridRow);
    rows.map(api => gridDAO.put(api));

    const ctx = foam.createSubContext({
      gridDAO,
      selectedCols,
    });
    parser = org.chromium.parse.GridQueryParser.create({
      of: org.chromium.apis.web.GridRow,
      interpreter: org.chromium.parse.ReleaseApiGridQueryInterpreter.create(
          null, ctx),
    }, ctx);
  });

  it('should match all APIs with interface-connector query', done => {
    const q = parser.parseString('#');
    gridDAO.where(q).select().then(arraySink => {
      expect(arraySink.array.length).toBe(rows.length);
    }).then(done).catch(done.fail);
  });

  it('should match keywords case-insensitively', done => {
    const q = parser.parseString('A');
    gridDAO.where(q).select().then(arraySink => {
      expect(arraySink.array.length).toBe(rows.length);
    }).then(done).catch(done.fail);
  });

  it('should match count of truthy selected cols', done => {
    const q = parser.parseString('count:2');
    gridDAO.where(q).select().then(arraySink => {
      expect(foam.util.equals(
          arraySink.array,
          foam.json.parse([
            {id: "Alpha#a", data: [F, T, T, T, T]},
            {id: "Alpha#b", data: [T, F, T, T, T]},
            {id: "Beta#y",  data: [T, T, T, T, F]},
            {id: "Gamma#q", data: [T, T, F, F, F]},
          ], org.chromium.apis.web.GridRow))).toBe(true);
    }).then(done).catch(done.fail);
  });

  it('should conjunct keyword and count queries', done => {
    const q = parser.parseString('count:2 amma');
    gridDAO.where(q).select().then(arraySink => {
      expect(foam.util.equals(
          arraySink.array,
          foam.json.parse([
            {id: "Gamma#q", data: [T, T, F, F, F]},
          ], org.chromium.apis.web.GridRow))).toBe(true);
    }).then(done).catch(done.fail);
  });
});
