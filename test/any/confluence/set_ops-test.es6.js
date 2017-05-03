// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

describe('Set ops', () => {
  describe('SetMinus', () => {
    var MDAO;
    var SetMinus;
    var E;
    var Num;
    var mkNum;
    var EvilDAO;
    var ErrorDAO;
    beforeEach(() => {
      MDAO = foam.lookup('foam.dao.MDAO');
      SetMinus = foam.lookup('org.chromium.mlang.sink.SetMinus');
      E = foam.lookup('foam.mlang.ExpressionsSingleton').create();

      foam.CLASS({
        package: 'org.chromium.mlang.sink.test',
        name: 'Num',

        properties: [
          {
            class: 'Int',
            name: 'id',
          },
        ],
      });

      Num = foam.lookup('org.chromium.mlang.sink.test.Num');
      mkNum = n => Num.create({id: n});

      foam.CLASS({
        package: 'org.chromium.mlang.sink.test',
        name: 'EvilDAO',
        extends: 'foam.dao.ProxyDAO',

        properties: [
          {
            class: 'Boolean',
            name: 'slow',
          },
        ],

        methods: [
          function find(objOrId) {
            var promise;
            if (this.slow) {
              promise = new Promise(
                  (resolve, reject) => setTimeout(
                      () => this.delegate.find(objOrId).then(resolve, reject),
                      100));
            } else {
              promise = this.delegate.find(objOrId);
            }

            this.slow = !this.slow;
            return promise;
          },
        ],
      });

      EvilDAO = foam.lookup('org.chromium.mlang.sink.test.EvilDAO');

      foam.CLASS({
        package: 'org.chromium.mlang.sink.test',
        name: 'ErrorDAO',
        extends: 'foam.dao.ProxyDAO',

        methods: [
          function select() {
            return Promise.reject(new Error('ErrorDAO: Error on select()'));
          },
          function find() {
            return Promise.reject(new Error('ErrorDAO: Error on find()'));
          },
        ],
      });

      ErrorDAO = foam.lookup('org.chromium.mlang.sink.test.ErrorDAO');
    });

    it('should yield full set on SET_MINUS(nullSet)', done => {
      var set = MDAO.create({of: Num});
      var nullSet = MDAO.create({of: Num});

      for (var i = 0; i < 10; i++) {
        set.put(mkNum(i));
      }

      set.orderBy(Num.ID).select(E.SET_MINUS(nullSet))
          .then(sink => {
            expect(sink.a.map(num => num.id))
                .toEqual([0, 1, 2, 3, 4, 5, 6, 7, 8, 9]);
            done();
          });
    });

    it('should remove all items when sets are identical', done => {
      var set = MDAO.create({of: Num});
      var subtrahend = MDAO.create({of: Num});

      for (var i = 0; i < 10; i++) {
        set.put(mkNum(i));
        subtrahend.put(mkNum(i));
      }

      set.select(E.SET_MINUS(subtrahend))
          .then(sink => {
            expect(sink.a).toEqual([]);

            done();
          });
    });

    it('should remove some items when there is overlap', done => {
      var set = MDAO.create({of: Num});
      var subtrahend = MDAO.create({of: Num});

      for (var i = 0; i < 10; i++) {
        set.put(mkNum(i));
        if ((i % 2) === 0) {
          subtrahend.put(mkNum(i));
        }
      }

      // {0..9} \ {0, 2, 4, 6, 8}
      set.orderBy(Num.ID).select(E.SET_MINUS(subtrahend))
          .then(sink => {
            expect(sink.a.map(num => num.id))
                .toEqual([1, 3, 5, 7, 9]);

            done();
          });
    });

    it('should ignore extra items in subtrahend', done => {
      var set = MDAO.create({of: Num});
      var subtrahend = MDAO.create({of: Num});

      for (var i = 0; i < 10; i++) {
        set.put(mkNum(i));
      }
      subtrahend.put(mkNum(9));
      subtrahend.put(mkNum(42));

      set.orderBy(Num.ID).select(E.SET_MINUS(subtrahend))
          .then(sink => {
            expect(sink.a.map(num => num.id))
                .toEqual([0, 1, 2, 3, 4, 5, 6, 7, 8]);

            done();
          });
    });

    it('should deliver items in order, even when subtrahend would resolve find()s out of order', done => {
      var set = MDAO.create({of: Num});
      var subtrahend = EvilDAO.create({
        of: Num,
        delegate: MDAO.create({of: Num}),
      });

      for (var i = 0; i < 10; i++) {
        set.put(mkNum(i));
        if ((i % 2) === 0) {
          subtrahend.put(mkNum(i));
        }
      }

      // {0..9} \ {0, 2, 4, 6, 8}
      set.orderBy(Num.ID).select(E.SET_MINUS(subtrahend))
          .then(sink => {
            expect(sink.a.map(num => num.id))
                .toEqual([1, 3, 5, 7, 9]);

            done();
          });
    });

    it('should reject when minuend rejects select()', done => {
      var set = ErrorDAO.create({of: Num, delegate: MDAO.create({of: Num})});
      var subtrahend = MDAO.create({of: Num});

      for (var i = 0; i < 10; i++) {
        set.put(mkNum(i));
      }

      set.select(E.SET_MINUS(subtrahend))
          .then(done.fail, done);
    });

    it('should reject when subtrahend rejects find()', done => {
      var set = MDAO.create({of: Num});
      var subtrahend = ErrorDAO.create({
        of: Num,
        delegate: MDAO.create({of: Num}),
      });

      for (var i = 0; i < 10; i++) {
        set.put(mkNum(i));
      }

      set.select(E.SET_MINUS(subtrahend))
          .then(done.fail, done);
    });
  });
});
