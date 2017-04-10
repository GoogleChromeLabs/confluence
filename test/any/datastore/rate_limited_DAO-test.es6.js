// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

describe('RateLimitedDAO', function() {
  let rateLimitedDAO;
  let arrayDAO;
  beforeEach(function() {
    jasmine.clock().install();
    arrayDAO = foam.dao.ArrayDAO.create();
    rateLimitedDAO = org.chromium.apis.web.RateLimitedDAO.create({
      interval: 50,
      delegate: arrayDAO,
    });
    foam.CLASS({
      package: 'test.RateLimitedDAO',
      name: 'Item',
      properties: ['id'],
    });
  });

  afterEach(function() {
    jasmine.clock().uninstall();
  });

  it('puts objects at specified interval.', function(done) {
    rateLimitedDAO.put(test.RateLimitedDAO.Item.create({id: 1}));
    rateLimitedDAO.put(test.RateLimitedDAO.Item.create({id: 2}));
    rateLimitedDAO.put(test.RateLimitedDAO.Item.create({id: 3}));
    jasmine.clock().tick(55);
    arrayDAO.select().then((arraySink) => {
      expect(arraySink.a.length).toBe(1);
    });
    jasmine.clock().tick(50);
    arrayDAO.select().then((arraySink) => {
      expect(arraySink.a.length).toBe(2);
    });
    jasmine.clock().tick(50);
    arrayDAO.select().then((arraySink) => {
      expect(arraySink.a.length).toBe(3);
    });
    done();
  });
  it('sucessfully puts Items into its delegate DAO.', function(done) {
    rateLimitedDAO.put(test.RateLimitedDAO.Item.create({id: 1}));
    rateLimitedDAO.put(test.RateLimitedDAO.Item.create({id: 2}));
    rateLimitedDAO.put(test.RateLimitedDAO.Item.create({id: 3}));
    jasmine.clock().tick(160);
    arrayDAO.select().then((arraySink) => {
      expect(arraySink.a.map((Item) => Item.id)).toEqual(
        [1, 2, 3]);
        done();
    });
  });
  it('accepts puts after partial dequeue and empties queue correctly.',
    function(done) {
      rateLimitedDAO.put(test.RateLimitedDAO.Item.create({id: 1}));
      rateLimitedDAO.put(test.RateLimitedDAO.Item.create({id: 2}));
      rateLimitedDAO.put(test.RateLimitedDAO.Item.create({id: 3}));
      jasmine.clock().tick(70);
      arrayDAO.select().then((arraySink) => {
        expect(arraySink.a.map((Item) => Item.id)).toEqual(
          [1]);
          done();
      });
      rateLimitedDAO.put(test.RateLimitedDAO.Item.create({id: 4}));
      rateLimitedDAO.put(test.RateLimitedDAO.Item.create({id: 5}));
      jasmine.clock().tick(150);
      arrayDAO.select().then((arraySink) => {
        expect(arraySink.a.map((Item) => Item.id)).toEqual(
          [1, 2, 3, 4]);
          done();
      });
      rateLimitedDAO.put(test.RateLimitedDAO.Item.create({id: 6}));
      rateLimitedDAO.put(test.RateLimitedDAO.Item.create({id: 7}));
      rateLimitedDAO.put(test.RateLimitedDAO.Item.create({id: 8}));
      jasmine.clock().tick(300);
      arrayDAO.select().then((arraySink) => {
        expect(arraySink.a.map((Item) => Item.id)).toEqual(
          [1, 2, 3, 4, 5, 6, 7, 8]);
          done();
      });
  });
});
