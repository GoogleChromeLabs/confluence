// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

describe('RateLimiter', function() {
  let rateLimiter;
  beforeEach(function() {
    jasmine.clock().install();
    rateLimiter = org.chromium.apis.web.RateLimiter.create({
      interval: 50,
    });
  });

  afterEach(function() {
    jasmine.clock().uninstall();
  });

  it('calls function at specified interval.', function() {
    let task = jasmine.createSpy('task');
    rateLimiter.enqueue(task);
    rateLimiter.enqueue(task);
    rateLimiter.enqueue(task);
    rateLimiter.enqueue(task);
    jasmine.clock().tick(55);
    expect(task).toHaveBeenCalled();
    expect(task.calls.count()).toEqual(1);
    jasmine.clock().tick(50);
    expect(task.calls.count()).toEqual(2);
    jasmine.clock().tick(50);
    expect(task.calls.count()).toEqual(3);
    jasmine.clock().tick(50);
    expect(task.calls.count()).toEqual(4);
  });

  it('handles more complex scenariou.', function() {
    let task1 = jasmine.createSpy('task1');
    let task2 = jasmine.createSpy('task2');
    let task3 = jasmine.createSpy('task3');
    let task4 = jasmine.createSpy('task4');
    let task5 = jasmine.createSpy('task5');
    rateLimiter.enqueue(task1);
    rateLimiter.enqueue(task2);
    rateLimiter.enqueue(() => {
      rateLimiter.enqueue(task3);
      rateLimiter.enqueue(task4);
    });
    jasmine.clock().tick(55);
    expect(task1).toHaveBeenCalled();
    jasmine.clock().tick(50);
    expect(task2).toHaveBeenCalled();
    jasmine.clock().tick(100);
    expect(task3).toHaveBeenCalled();
    jasmine.clock().tick(50);
    expect(task4).toHaveBeenCalled();
    rateLimiter.enqueue(task5);
    jasmine.clock().tick(50);
    expect(task5).toHaveBeenCalled();
  });
});
