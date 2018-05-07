// Copyright 2018 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

// TODO(markdittmer): Should probably more consistently replace dependent DAOs
// with fake implementations.
describe('ApiServiceDAO', () => {
  let pkg;
  let ctx;
  beforeEach(() => {
    pkg = org.chromium.apis.web;

    foam.CLASS({
      name: 'Thing',
      package: 'org.chromium.apis.web.test',

      properties: ['id'],
    });

    foam.CLASS({
      name: 'FakeContainer',
      package: 'org.chromium.apis.web.test',

      exports: ['as container'],
    });
    ctx = foam.box.Context.create(null, pkg.test.FakeContainer.create())
        .__subContext__;
  });

  it('should require baseURL', () => {
    expect(() => pkg.ApiServiceDAO.create({
      of: pkg.test.Thing,
      workerRegistry: foam.box.Context.create(null, ctx).registry,
    }, ctx)
        // Access delegate to trigger lazy factory responsible for validate().
        .delegate).toThrow();
  },);

  it('should require workerRegistry', () => {
    expect(() => pkg.ApiServiceDAO.create({
      of: pkg.test.Thing,
      baseURL: 'https://example.com/thingDAO',
    }, ctx)
        // Access delegate to trigger lazy factory responsible for validate().
        .delegate).toThrow();
  });

  it('should instantiate with all requirements', () => {
    expect(() => pkg.ApiServiceDAO.create({
      of: pkg.test.Thing,
      baseURL: 'https://example.com/thingDAO',
      workerRegistry: foam.box.Context.create(null, ctx).registry,
    }, ctx)
        // Access delegate to trigger lazy factory responsible for validate().
        .delegate).not.toThrow();
  });

  it('should pass baseURL to WorkerDAO', () => {
    const baseURL = 'https://example.com/thingDAO';
    let numWorkerDAOs = 0;
    foam.CLASS({
      name: 'MockWorkerDAO',
      package: 'org.chromium.apis.web.test',
      extends: 'org.chromium.apis.web.WorkerDAO',

      properties: [
        {
          class: 'Int',
          name: 'baseURLSetCount',
        },
        {
          name: 'baseURL',
          postSet: function(_, nu) {
            expect(nu).toBe(baseURL);
          },
        },
      ],

      methods: [
        function init() {
          this.SUPER();
          numWorkerDAOs++;
          expect(this.baseURL).toBe(baseURL);
        },
      ],
    });

    ctx.register(pkg.test.MockWorkerDAO, 'org.chromium.apis.web.WorkerDAO');

    pkg.ApiServiceDAO.create({
      of: pkg.test.Thing,
      baseURL,
      workerRegistry: foam.box.Context.create(null, ctx).registry,
    }, ctx)
        // Access delegate to trigger lazy factory.
        .delegate;

    // Registry may involve instantiating more than one WorkerDAO, but at least
    // one should be created.
    expect(numWorkerDAOs).toBeGreaterThan(0);
  });

  it('should register worker DAO under given name', () => {
    const serviceName = 'serviceName';
    let numRegisters = 0;
    foam.CLASS({
      name: 'MockBoxRegistry',
      package: 'org.chromium.apis.web.test',
      extends: 'foam.box.BoxRegistryBox',

      methods: [
        function register(name, service, box) {
          expect(name).toBe(serviceName);
          numRegisters++;
          return this.SUPER(name, service, box);
        },
      ],
    });

    const workerCtx = foam.createSubContext({});
    workerCtx.register(pkg.test.MockBoxRegistry, 'foam.box.BoxRegistryBox');
    const workerRegistry = foam.box.Context.create(null, workerCtx).registry;

    pkg.ApiServiceDAO.create({
      of: pkg.test.Thing,
      name: serviceName,
      baseURL: 'https://example.com/thingDAO',
      workerRegistry,
    }, ctx)
        // Access delegate to trigger lazy factory.
        .delegate;

    expect(numRegisters).toBeGreaterThan(0);
  });

  it('should change delegate after async select()', done => {
    const workerCtx = foam.createSubContext({});
    const overwriteRegister = (cls, id) => {
      ctx.register(cls, id);
      workerCtx.register(cls, id);
    }
    overwriteRegister(foam.dao.NullDAO, 'org.chromium.apis.web.WorkerDAO');
    overwriteRegister(foam.dao.NullDAO, 'foam.dao.RestDAO');

    const dao = pkg.ApiServiceDAO.create({
      of: pkg.test.Thing,
      baseURL: 'https://example.com/thingDAO',
      workerRegistry: foam.box.Context.create(null, workerCtx).registry,
    }, ctx);
    const initialDelegate = dao.delegate;

    // Assert that a property change event is fired by dao.delegate$ property
    // slot. This test will time out (because done() isn't called) if
    // dao.delegate is not set after this subscription is established.
    dao.delegate$.sub(done);
  });
});
