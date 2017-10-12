// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

describe('HttpJsonDAO', function() {
  let pkg;
  let ctx;
  const EXPECTED_URL = 'https://storage.googleapis.com/web-api-confluence-data-cache/test';
  beforeAll(() => {
    pkg = org.chromium.apis.web;
  });

  function declareReplayClass(name, replayStr) {
    foam.CLASS({
      package: 'test',
      name,

      requires: ['foam.net.HTTPResponse'],

      constants: {EXPECTED_URL},

      properties: [
        {
          class: 'String',
          name: 'url',
        },
        {
          class: 'String',
          name: 'responseType',
        },
      ],

      methods: [
        function send() {
          if (this.url !== this.EXPECTED_URL)
            throw new Error(`Unexpected HTTP request to URL: ${this.url}`);

          return Promise.resolve(this.HTTPResponse.create({
            status: 200,
            payload: Promise.resolve(replayStr),
          }));
        },
      ],
    });
  }

  function createHttpJsonDAO(classWhitelist, url, ctx) {
    return pkg.HttpJsonDAO.create({
      url,
      parser: foam.json.Parser.create({
        strict: true,
        creationContext: foam.box.ClassWhitelistContext.create({
          whitelist: classWhitelist,
        }, ctx).__subContext__,
      }, ctx),
    }, ctx);
  }

  it('should load empty array from HTTPRequest', done => {
    let ctx = foam.__context__.createSubContext();
    declareReplayClass('EmptyArrayHTTPRequest', JSON.stringify([]));
    ctx.register(test.EmptyArrayHTTPRequest, 'foam.net.HTTPRequest');

    createHttpJsonDAO([], EXPECTED_URL, ctx).select().then(arraySink => {
      expect(arraySink.array.length).toBe(0);
    }).then(done, done.fail);
  });

  it('should load non-empty array from HTTPRequest', done => {
    foam.CLASS({
      package: 'test',
      name: 'Item',

      properties: [{class: 'Int', name: 'id'}],
    });

    let ctx = foam.__context__.createSubContext();
    const array = [
      test.Item.create({id: 0}),
      test.Item.create({id: 1}),
      test.Item.create({id: 2}),
    ];
    declareReplayClass('NonEmptyArrayHTTPRequest',
                       foam.json.Strict.stringify(array));
    ctx.register(test.NonEmptyArrayHTTPRequest, 'foam.net.HTTPRequest');

    createHttpJsonDAO(['test.Item'], EXPECTED_URL, ctx).select()
        .then(arraySink => {
          expect(foam.util.equals(array, arraySink.array)).toBe(true);
        }).then(done, done.fail);
  });

  it('should fail when item class is not whitelisted', done => {
    foam.CLASS({
      package: 'test',
      name: 'Item',

      properties: [{class: 'Int', name: 'id'}],
    });

    let ctx = foam.__context__.createSubContext();
    const array = [test.Item.create({id: 0})];
    declareReplayClass('NonEmptyArrayHTTPRequest',
                       foam.json.Strict.stringify(array));
    ctx.register(test.NonEmptyArrayHTTPRequest, 'foam.net.HTTPRequest');

    createHttpJsonDAO([], EXPECTED_URL, ctx).select().then(done.fail, done);
  });
});
