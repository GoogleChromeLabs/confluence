// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

describe('HttpJsonDAO', () => {
  let pkg;
  beforeAll(() => {
    pkg = org.chromium.apis.web;
  });

  function declareReplayClass(name, replayStr) {
    foam.CLASS({
      package: 'test',
      name,

      requires: ['foam.net.HTTPResponse'],

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
          return Promise.resolve(this.HTTPResponse.create({
            status: 200,
            payload: Promise.resolve(replayStr),
          }));
        },
      ],
    });
    foam.CLASS({
      package: 'test',
      name: 'NullItem',

      properties: ['id'],
    });
  }

  function createHttpJsonDAO(classWhitelist, ctx, opt_args) {
    const daoArgs = Object.assign({
      of: test.NullItem,
      url: 'https://safe.com/some/resource',
      safeProtocols: ['https:'],
      safeHostnames: ['safe.com'],
      safePathPrefixes: ['/'],
      parser: foam.json.Parser.create({
        strict: true,
        creationContext: foam.box.ClassWhitelistContext.create({
          whitelist: classWhitelist,
        }, ctx).__subContext__,
      }, ctx),
    }, opt_args || {});
    return pkg.HttpJsonDAO.create(daoArgs, ctx);
  }

  it('should load empty array from HTTPRequest', done => {
    let ctx = foam.__context__.createSubContext();
    declareReplayClass('EmptyArrayHTTPRequest', JSON.stringify([]));
    ctx.register(test.EmptyArrayHTTPRequest, 'foam.net.HTTPRequest');

    createHttpJsonDAO([], ctx).select().then(arraySink => {
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

    createHttpJsonDAO(['test.Item'], ctx).select()
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

    createHttpJsonDAO([], ctx).select().then(done.fail, done);
  });

  it('should succeed when URL is safe, using custom safety params', done => {
    let ctx = foam.__context__.createSubContext();
    declareReplayClass('EmptyArrayHTTPRequest', JSON.stringify([]));
    ctx.register(test.EmptyArrayHTTPRequest, 'foam.net.HTTPRequest');

    createHttpJsonDAO([], ctx, {
      url: 'http://json.safe.com/data/0',
      safeProtocols: ['http:'],
      safeHostnames: ['json.safe.com'],
      safePathPrefixes: ['/data'],
    }).select().then(done, done.fail);
  });

  it('should fail when URL is unsafe', done => {
    let ctx = foam.__context__.createSubContext();
    declareReplayClass('EmptyArrayHTTPRequest', JSON.stringify([]));
    ctx.register(test.EmptyArrayHTTPRequest, 'foam.net.HTTPRequest');

    createHttpJsonDAO([], ctx, {
      url: 'https://evil.com/something/maniacal',
    }).select().then(done.fail, done);
  });

  it('should not issue requests or instantiate delegate, in Serializable flavour', done => {
    let ctx = foam.__context__.createSubContext();
    foam.CLASS({
      package: 'test',
      name: 'ErrorHTTPRequest',

      methods: [
        function send() {
          return Promise.reject(
              new Error('Expected no HTTPRequest.send() calls'));
        },
      ],
    });
    ctx.register(test.ErrorHTTPRequest, 'foam.net.HTTPRequest');

    foam.CLASS({
      package: 'test',
      name: 'Item',

      properties: [{class: 'Int', name: 'id'}],
    });
    foam.CLASS({
      package: 'test',
      name: 'NoDelegateHttpJsonDAO',
      extends: 'org.chromium.apis.web.SerializableHttpJsonDAO',

      properties: [
        {
          name: 'delegate',
          preSet: function() {
            throw new Error('Expected no instantiation of delegate');
          },
          factory: function() { return foam.dao.NullDAO.create(); },
        },
      ],
    });

    // Check that safeguards actually work.
    pkg.HttpJsonDAO.create({
      url: 'http://localhost:8080/',
      safeProtocols: ['http:'],
      safeHostnames: ['localhost'],
      safePathPrefixes: ['/'],
    }, ctx).promise.then(done.fail, error => {
      expect(error.message).toBe('Expected no HTTPRequest.send() calls');
      try {
        test.NoDelegateHttpJsonDAO.create({
          url: 'http://localhost:8080/',
          safeProtocols: ['http:'],
          safeHostnames: ['localhost'],
          safePathPrefixes: ['/'],
        }, ctx).delegate;
        done.fail('Expected throw on instantiation of delegate');
      } catch (error) {
        expect(error.message).toBe('Expected no instantiation of delegate');

        // Perform the test:
        try {
          // Stringifying serializable flavour of DAO should not trigger errors
          // baked into test classes.
          foam.json.Network.stringify(test.NoDelegateHttpJsonDAO.create({
            of: test.Item,
            url: 'https://example.com/some/resource',
          }, ctx));
          done();
        } catch (error) {
          done.fail(error);
        }
      }
    });
  });
});
