// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

describe('Set ops', () => {
  let MDAO;
  let E;
  let Num;
  let mkNum;
  let EvilDAO;
  let ErrorDAO;
  beforeEach(() => {
    MDAO = foam.lookup('foam.dao.MDAO');
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
    mkNum = (n) => Num.create({id: n});

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
          let promise;
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

  describe('AsyncSink', () => {
    it('should reject when minuend rejects select()', (done) => {
      const set = ErrorDAO.create({of: Num, delegate: MDAO.create({of: Num})});
      const subtrahend = MDAO.create({of: Num});

      for (let i = 0; i < 10; i++) {
        set.put(mkNum(i));
      }

      set.select(E.SET_MINUS(subtrahend))
          .then(done.fail, (error) => {
            expect(error.message).toBe('ErrorDAO: Error on select()');
            done();
          });
    });

    it('should reject when subtrahend rejects find()', (done) => {
      const set = MDAO.create({of: Num});
      const subtrahend = ErrorDAO.create({
        of: Num,
        delegate: MDAO.create({of: Num}),
      });

      for (let i = 0; i < 10; i++) {
        set.put(mkNum(i));
      }

      set.select(E.SET_MINUS(subtrahend))
          .then(done.fail, (error) => {
            expect(error.message).toBe('ErrorDAO: Error on find()');
          }).then(done, done.fail);
    });
  });

  describe('SetMinus', () => {
    it('should yield full set on SET_MINUS(nullSet)', () => {
      const set = MDAO.create({of: Num});
      const nullSet = MDAO.create({of: Num});

      for (let i = 0; i < 10; i++) {
        set.put(mkNum(i));
      }

      return set.orderBy(Num.ID).select(E.SET_MINUS(nullSet))
          .then((sink) => {
            expect(sink.array.map((num) => num.id))
                .toEqual([0, 1, 2, 3, 4, 5, 6, 7, 8, 9]);
          });
    });

    it('should exclude all items when sets are identical', () => {
      const set = MDAO.create({of: Num});
      const subtrahend = MDAO.create({of: Num});

      for (let i = 0; i < 10; i++) {
        set.put(mkNum(i));
        subtrahend.put(mkNum(i));
      }

      return set.select(E.SET_MINUS(subtrahend))
          .then((sink) => {
            expect(sink.array).toEqual([]);
          });
    });

    it('should exclude some items when there is overlap', () => {
      const set = MDAO.create({of: Num});
      const subtrahend = MDAO.create({of: Num});

      for (let i = 0; i < 10; i++) {
        set.put(mkNum(i));
        if ((i % 2) === 0) {
          subtrahend.put(mkNum(i));
        }
      }

      // {0..9} \ {0, 2, 4, 6, 8}
      return set.orderBy(Num.ID).select(E.SET_MINUS(subtrahend))
          .then((sink) => {
            expect(sink.array.map((num) => num.id))
                .toEqual([1, 3, 5, 7, 9]);
          });
    });

    it('should support composition', () => {
      const set = MDAO.create({of: Num});
      const subtrahend1 = MDAO.create({of: Num});
      const subtrahend2 = MDAO.create({of: Num});

      for (let i = 0; i < 10; i++) {
        set.put(mkNum(i));
      }

      subtrahend1.put(mkNum(1));
      subtrahend1.put(mkNum(7));

      subtrahend2.put(mkNum(3));
      subtrahend2.put(mkNum(5));
      subtrahend2.put(mkNum(9));

      // {0..9} \ {1, 7} \ {3, 5, 9}
      return set.orderBy(Num.ID)
          .select(E.SET_MINUS(subtrahend1, E.SET_MINUS(subtrahend2)))
          .then((sink) => {
            expect(sink.array.map((num) => num.id))
                .toEqual([0, 2, 4, 6, 8]);
          });
    });

    it('should support array-of-DAOs', (done) => {
      const set = MDAO.create({of: Num});
      const subtrahend1 = MDAO.create({of: Num});
      const subtrahend2 = MDAO.create({of: Num});

      for (let i = 0; i < 10; i++) {
        set.put(mkNum(i));
      }

      subtrahend1.put(mkNum(1));
      subtrahend1.put(mkNum(7));

      subtrahend2.put(mkNum(3));
      subtrahend2.put(mkNum(5));
      subtrahend2.put(mkNum(9));

      // {0..9} \ {1, 7} \ {3, 5, 9}
      set.orderBy(Num.ID).select(E.SET_MINUS([subtrahend1, subtrahend2]))
          .then((sink) => {
            expect(sink.array.map((num) => num.id))
                .toEqual([0, 2, 4, 6, 8]);
          }).then(done, done.fail);
    });

    it('should ignore extra items in subtrahend', (done) => {
      const set = MDAO.create({of: Num});
      const subtrahend = MDAO.create({of: Num});

      for (let i = 0; i < 10; i++) {
        set.put(mkNum(i));
      }
      subtrahend.put(mkNum(9));
      subtrahend.put(mkNum(42));

      set.orderBy(Num.ID).select(E.SET_MINUS(subtrahend))
          .then((sink) => {
            expect(sink.array.map((num) => num.id))
                .toEqual([0, 1, 2, 3, 4, 5, 6, 7, 8]);
          }).then(done, done.fail);
    });

    it('should deliver items in order, even when subtrahend would resolve find()s out of order', (done) => {
      const set = MDAO.create({of: Num});
      const subtrahend = EvilDAO.create({
        of: Num,
        delegate: MDAO.create({of: Num}),
      });

      for (let i = 0; i < 10; i++) {
        set.put(mkNum(i));
        if ((i % 2) === 0) {
          subtrahend.put(mkNum(i));
        }
      }

      // {0..9} \ {0, 2, 4, 6, 8}
      set.orderBy(Num.ID).select(E.SET_MINUS(subtrahend))
          .then((sink) => {
            expect(sink.array.map((num) => num.id))
                .toEqual([1, 3, 5, 7, 9]);
          }).then(done, done.fail);
    });
  });

  describe('Intersect', () => {
    it('should yield full set on INTERSECT(sameSet)', (done) => {
      const set = MDAO.create({of: Num});
      const sameSet = MDAO.create({of: Num});

      for (let i = 0; i < 10; i++) {
        set.put(mkNum(i));
        sameSet.put(mkNum(i));
      }

      set.orderBy(Num.ID).select(E.INTERSECT(sameSet))
          .then((sink) => {
            expect(sink.array.map((num) => num.id))
                .toEqual([0, 1, 2, 3, 4, 5, 6, 7, 8, 9]);
          }).then(done, done.fail);
    });

    it('should yield null set on INTERSECT(nullSet)', (done) => {
      const set = MDAO.create({of: Num});
      const nullSet = MDAO.create({of: Num});

      for (let i = 0; i < 10; i++) {
        set.put(mkNum(i));
      }

      set.orderBy(Num.ID).select(E.INTERSECT(nullSet))
          .then((sink) => {
            expect(sink.array.map((num) => num.id)).toEqual([]);
          }).then(done, done.fail);
    });

    it('should exclude some items when secondary is subset', (done) => {
      const set = MDAO.create({of: Num});
      const secondary = MDAO.create({of: Num});

      for (let i = 0; i < 10; i++) {
        set.put(mkNum(i));
        if ((i % 2) === 0) {
          secondary.put(mkNum(i));
        }
      }

      // {0..9} INTERSECT {0, 2, 4, 6, 8}
      set.orderBy(Num.ID).select(E.INTERSECT(secondary))
          .then((sink) => {
            expect(sink.array.map((num) => num.id))
                .toEqual([0, 2, 4, 6, 8]);
          }).then(done, done.fail);
    });

    it('should exclude some items when primary is subset', (done) => {
      const set = MDAO.create({of: Num});
      const secondary = MDAO.create({of: Num});

      for (let i = 0; i < 10; i++) {
        if ((i % 2) === 0) {
          set.put(mkNum(i));
        }

        secondary.put(mkNum(i));
      }

      // {0, 2, 4, 6, 8} INTERSECT {0..9}
      set.orderBy(Num.ID).select(E.INTERSECT(secondary))
          .then((sink) => {
            expect(sink.array.map((num) => num.id))
                .toEqual([0, 2, 4, 6, 8]);
          }).then(done, done.fail);
    });

    it('should support composition', (done) => {
      const set = MDAO.create({of: Num});
      const secondary1 = MDAO.create({of: Num});
      const secondary2 = MDAO.create({of: Num});

      for (let i = 0; i < 10; i++) {
        set.put(mkNum(i));
      }

      secondary1.put(mkNum(1));
      secondary1.put(mkNum(7));

      secondary2.put(mkNum(3));
      secondary2.put(mkNum(7));
      secondary2.put(mkNum(5));

      // {0..9} INTERSECT {1, 7} INTERSECT {3, 7, 5}
      set.orderBy(Num.ID)
          .select(E.INTERSECT(secondary1, E.INTERSECT(secondary2)))
          .then((sink) => {
            expect(sink.array.map((num) => num.id))
                .toEqual([7]);
          }).then(done, done.fail);
    });

    it('should support array-of-DAOs', (done) => {
      const set = MDAO.create({of: Num});
      const secondary1 = MDAO.create({of: Num});
      const secondary2 = MDAO.create({of: Num});

      for (let i = 0; i < 10; i++) {
        set.put(mkNum(i));
      }

      secondary1.put(mkNum(1));
      secondary1.put(mkNum(7));

      secondary2.put(mkNum(3));
      secondary2.put(mkNum(7));
      secondary2.put(mkNum(5));

      // {0..9} INTERSECT {1, 7} INTERSECT {3, 7, 5}
      set.orderBy(Num.ID)
          .select(E.INTERSECT([secondary1, secondary2]))
          .then((sink) => {
            expect(sink.array.map((num) => num.id))
                .toEqual([7]);
          }).then(done, done.fail);
    });

    it('should deliver items in order, even when secondary would resolve find()s out of order', (done) => {
      const set = MDAO.create({of: Num});
      const secondary = EvilDAO.create({
        of: Num,
        delegate: MDAO.create({of: Num}),
      });

      for (let i = 0; i < 10; i++) {
        set.put(mkNum(i));
        if ((i % 2) === 0) {
          secondary.put(mkNum(i));
        }
      }

      // {0..9} INTERSECT {0, 2, 4, 6, 8}
      set.orderBy(Num.ID).select(E.INTERSECT(secondary))
          .then((sink) => {
            expect(sink.array.map((num) => num.id))
                .toEqual([0, 2, 4, 6, 8]);
          }).then(done, done.fail);
    });
  });

  describe('Union', () => {
    it('should throw on put(), remove(), removeAll()', () => {
      const primary = MDAO.create({of: Num});
      const secondary = MDAO.create({of: Num});
      expect(() => E.UNION(primary, secondary).put(mkNum(0))).toThrow();
      expect(() => E.UNION(primary, secondary).remove(mkNum(0))).toThrow();
      expect(() => E.UNION(primary, secondary).removeAll(mkNum(0))).toThrow();
    });

    it('should throw on select()/listen() with skip/limit/order/predicate', () => {
      const primary = MDAO.create({of: Num});
      const secondary = MDAO.create({of: Num});
      const u = undefined;
      expect(() => E.UNION(primary, secondary).select(u, 1)).toThrow();
      expect(() => E.UNION(primary, secondary).select(u, u, 1)).toThrow();
      expect(() => E.UNION(primary, secondary).select(u, u, u, Num.ID))
          .toThrow();
      expect(() => E.UNION(primary, secondary)
          .select(u, u, u, u, E.EQ(Num.ID, 0))).toThrow();
      expect(() => E.UNION(primary, secondary).listen(u, 1)).toThrow();
      expect(() => E.UNION(primary, secondary).listen(u, u, 1)).toThrow();
      expect(() => E.UNION(primary, secondary).listen(u, u, u, Num.ID))
          .toThrow();
      expect(() => E.UNION(primary, secondary)
          .listen(u, u, u, u, E.EQ(Num.ID, 0))).toThrow();
    });

    it('should combine non-intersecting sets', (done) => {
      const primary = MDAO.create({of: Num});
      const secondary = MDAO.create({of: Num});

      primary.put(mkNum(0));
      secondary.put(mkNum(1));

      E.UNION(primary, secondary).select().then((sink) => {
        expect(sink.array.map((num) => num.id).sort()).toEqual([0, 1]);
      }).then(done, done.fail);
    });

    it('should combine intersecting sets', (done) => {
      const primary = MDAO.create({of: Num});
      const secondary = MDAO.create({of: Num});

      primary.put(mkNum(0));
      secondary.put(mkNum(0));
      secondary.put(mkNum(1));

      E.UNION(primary, secondary).select().then((sink) => {
        expect(sink.array.map((num) => num.id).sort()).toEqual([0, 1]);
      }).then(done, done.fail);
    });

    it('should be composable', (done) => {
      const primary = MDAO.create({of: Num});
      const secondary = MDAO.create({of: Num});
      const tertiary = MDAO.create({of: Num});

      primary.put(mkNum(0));
      secondary.put(mkNum(1));
      tertiary.put(mkNum(2));

      E.UNION(primary, E.UNION(secondary, tertiary)).select().then((sink) => {
        expect(sink.array.map((num) => num.id).sort()).toEqual([0, 1, 2]);
      }).then(done, done.fail);
    });

    it('should support more than 2 DAOs', (done) => {
      const primary = MDAO.create({of: Num});
      const secondary = MDAO.create({of: Num});
      const tertiary = MDAO.create({of: Num});

      primary.put(mkNum(0));
      secondary.put(mkNum(1));
      tertiary.put(mkNum(2));

      E.UNION(primary, secondary, tertiary).select().then((sink) => {
        expect(sink.array.map((num) => num.id).sort()).toEqual([0, 1, 2]);
      }).then(done, done.fail);
    });

    it('should compose correctly with inconsistent find() callback timing', (done) => {
      const aDAO = EvilDAO.create({of: Num, delegate: MDAO.create({of: Num})});
      const bDAO = EvilDAO.create({of: Num, delegate: MDAO.create({of: Num})});
      const cDAO = EvilDAO.create({of: Num, delegate: MDAO.create({of: Num})});

      aDAO.put(mkNum(0));
      aDAO.put(mkNum(1));
      aDAO.put(mkNum(2));
      aDAO.put(mkNum(3));

      bDAO.put(mkNum(3));
      bDAO.put(mkNum(4));
      bDAO.put(mkNum(5));

      cDAO.put(mkNum(0));
      cDAO.put(mkNum(4));
      cDAO.put(mkNum(6));

      E.UNION(aDAO, E.UNION(bDAO, cDAO)).select().then((sink) => {
        expect(sink.array.map((num) => num.id).sort()).toEqual([0, 1, 2, 3, 4, 5, 6]);
      }).then(done, done.fail);
    });

    it('should resolve null on find() with inconsistent callback timing', (done) => {
      const aDAO = EvilDAO.create({of: Num, delegate: MDAO.create({of: Num})});
      const bDAO = EvilDAO.create({of: Num, delegate: MDAO.create({of: Num})});
      const abDAO = E.UNION(aDAO, bDAO);

      Promise.all([
        abDAO.find(0).then((found) => expect(found).toBeNull()),
        abDAO.find(1).then((found) => expect(found).toBeNull()),
        abDAO.find(2).then((found) => expect(found).toBeNull()),
        abDAO.find(3).then((found) => expect(found).toBeNull()),
      ]).then(done, done.fail);
    });
  });
});
