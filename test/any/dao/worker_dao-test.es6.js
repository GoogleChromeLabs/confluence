// Copyright 2018 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

describe('WorkerDAO', () => {
  let pkg;
  let ctx;
  beforeEach(() => {
    pkg = org.chromium.apis.web;

    foam.CLASS({
      name: 'Thing',
      package: 'org.chromium.apis.web.test',

      properties: ['id'],
    });

    const containerCtx = foam.createSubContext({
      isContainerCtx: true,
    });
    foam.CLASS({
      name: 'FakeContainer',
      package: 'org.chromium.apis.web.test',

      exports: ['as container'],

      properties: [
        {
          name: 'ctx',
          value: containerCtx,
        },
      ],
    });
    ctx = pkg.test.FakeContainer.create();

    foam.CLASS({
      name: 'DelayedCountDAO',
      package: 'org.chromium.apis.web.test',
      extends: 'foam.dao.MDAO',

      properties: [
        {
          class: 'Int',
          name: 'selects',
        },
        {
          class: 'Int',
          name: 'listens',
        },
      ],

      methods: [
        function select() {
          return this.SUPER.apply(this, arguments).then(ret => {
            ++this.batchReads;
            return new Promise(resolve => setTimeout(() => resolve(ret), 100));
          });
        },
        function listen() {
          return this.SUPER.apply(this, arguments).then(ret => {
            ++this.batchReads;
            return new Promise(resolve => setTimeout(() => resolve(ret), 100));
          });
        },
      ],
    });

    foam.CLASS({
      name: 'DelayedCountDAOFactory',
      package: 'org.chromium.apis.web.test',
      implements: ['org.chromium.apis.web.DAOFactory'],

      requires: ['org.chromium.apis.web.test.DelayedCountDAO'],

      properties: [
        {
          name: 'latestDAO',
          value: null,
        },
      ],

      methods: [
        function create(opts, ctx) {
          return this.latestDAO = this.DelayedCountDAO.create(opts, ctx);;
        },
      ],
    });
  });

  it('should require container context', () => {
    expect(() => foam.json.stringify(pkg.WorkerDAO.create({
      of: pkg.test.Thing,
      baseURL: 'https://example.com/restDAO',
    }))).toThrow();
  });

  it('should require baseURL', () => {
    expect(() => foam.json.stringify(pkg.WorkerDAO.create({
      of: pkg.test.Thing,
    }, ctx))).toThrow();
  });

  it('should be serializable', () => {
    expect(() => foam.json.stringify(pkg.WorkerDAO.create({
      of: pkg.test.Thing,
      baseURL: 'https://example.com/restDAO',
    }, ctx))).not.toThrow();
  });

  it('select/listen against cache once during setup', done => {
    const factory = pkg.test.DelayedCountDAOFactory.create(null, ctx);
    const dao = pkg.WorkerDAO.create({
      of: pkg.test.Thing,
      baseURL: 'https://example.com/restDAO',
      remoteDAOFactory: factory,
    }, ctx);

    let setupSelects;
    let setupListens;
    dao.select().then(() => {
      // CachingDAO may select(), listen(), or both on remote.
      setupSelects = factory.latestDAO.selects;
      setupListens = factory.latestDAO.listens;
      expect(setupSelects).toBeLessThan(2);
      expect(setupListens).toBeLessThan(2);
      return Promise.all([
        dao.select(),
        dao.listen(foam.dao.ArraySink.create()),
      ]);
    }).then(() => {
      // No subsequent remote selects or listens to service requests.
      expect(factory.latestDAO.selects).toBe(setupSelects);
      expect(factory.latestDAO.listens).toBe(setupListens);
      done();
    });
  });
});
