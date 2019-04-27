// Copyright 2018 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

describe('WorkerDAO', () => {
  let pkg;
  let ctx;
  let latestDelayedCountDAO;
  beforeEach(() => {
    pkg = org.chromium.apis.web;

    foam.CLASS({
      name: 'Thing',
      package: 'org.chromium.apis.web.test',

      properties: ['id'],
    });

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
        function init() {
          this.SUPER();
          latestDelayedCountDAO = this;
        },
        function select() {
          return this.SUPER.apply(this, arguments).then((ret) => {
            ++this.batchReads;
            return new Promise((resolve) => setTimeout(() => resolve(ret), 100));
          });
        },
        function listen() {
          return this.SUPER.apply(this, arguments).then((ret) => {
            ++this.batchReads;
            return new Promise((resolve) => setTimeout(() => resolve(ret), 100));
          });
        },
      ],
    });

    // Replace RestDAO with mock DelayedCountDAO in WorkerDAO's localCtx.
    const localCtx = foam.createSubContext({});
    localCtx.register(pkg.test.DelayedCountDAO, 'foam.dao.RestDAO');

    foam.CLASS({
      name: 'FakeContainer',
      package: 'org.chromium.apis.web.test',

      exports: ['as container'],

      properties: [
        // WorkerDAO's default localCtx is its container ctx property.
        {
          name: 'ctx',
          value: localCtx,
        },
      ],
    });
    ctx = pkg.test.FakeContainer.create({});
  });

  it('should require baseURL', () => {
    expect(() => pkg.WorkerDAO.create({
      of: pkg.test.Thing,
    }, ctx)
        // Access delegate to trigger lazy factory responsible for validate().
        .delegate).toThrow();
  });

  it('should be serializable', () => {
    expect(() => foam.json.stringify(pkg.WorkerDAO.create({
      of: pkg.test.Thing,
    }, ctx))).not.toThrow();
  });

  it('select/listen against cache once during setup', (done) => {
    const dao = pkg.WorkerDAO.create({
      of: pkg.test.Thing,
      baseURL: 'https://example.com/restDAO',
    }, ctx);

    let setupSelects;
    let setupListens;
    dao.select().then(() => {
      // CachingDAO may select(), listen(), or both on remote.
      setupSelects = latestDelayedCountDAO.selects;
      setupListens = latestDelayedCountDAO.listens;
      expect(setupSelects).toBeLessThan(2);
      expect(setupListens).toBeLessThan(2);
      return Promise.all([
        dao.select(),
        dao.listen(foam.dao.ArraySink.create()),
      ]);
    }).then(() => {
      // No subsequent remote selects or listens to service requests.
      expect(latestDelayedCountDAO.selects).toBe(setupSelects);
      expect(latestDelayedCountDAO.listens).toBe(setupListens);
      done();
    });
  });
});
