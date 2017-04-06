// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

describe('RateLimitedDAO', function() {
  let rateLimitedDAO;
  foam.CLASS({
    package: 'test.RateLimitedDAO',
    name: 'item',
    properties: ['id'],
  });
  beforeEach(function() {
    rateLimitedDAO = org.chromium.apis.web.RateLimitedDAO.create({
      interval: 100,
      delegate: foam.dao.ArrayDAO.create(),
    });
  });
  it('puts objects at specified interval.', function(done) {
    rateLimitedDAO.put(test.RateLimitedDAO.item.create({id: 1}));
    rateLimitedDAO.put(test.RateLimitedDAO.item.create({id: 2}));
    rateLimitedDAO.put(test.RateLimitedDAO.item.create({id: 3}));
    setTimeout(() => {
      rateLimitedDAO.select().then((arraySink) => {
        expect(arraySink.a.length).toBe(1);
      });
    }, 100);
    setTimeout(() => {
      rateLimitedDAO.select().then((arraySink) => {
        expect(arraySink.a.length).toBe(2);
      });
    }, 200);
    setTimeout(() => {
      rateLimitedDAO.select().then((arraySink) => {
        expect(arraySink.a.length).toBe(3);
      });
    }, 300);
    setTimeout(() => {
      done();
    }, 500);
  });
  it('puts items into its delegate DAO.', function(done) {
    rateLimitedDAO.put(test.RateLimitedDAO.item.create({id: 1}));
    rateLimitedDAO.put(test.RateLimitedDAO.item.create({id: 2}));
    rateLimitedDAO.put(test.RateLimitedDAO.item.create({id: 3}));
    setTimeout(() => {
      rateLimitedDAO.select().then((arraySink) => {
        expect(arraySink.a.map((item) => item.id).sort()).toEqual(
          [1, 2, 3]);
          done();
      });
    }, 500);
  });
});
