// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

describe('RateLimiter', function() {
  let rateLimiter;
  beforeEach(function() {
    rateLimiter = org.chromium.apis.web.RateLimiter.create({
      interval: 100,
    });
  });
  it('calls function at specified interval.', function(done) {
    let task = jasmine.createSpy('task');
    rateLimiter.enqueue(task);
    rateLimiter.enqueue(task);
    rateLimiter.enqueue(task);
    rateLimiter.enqueue(task);
    setTimeout(() => {
      expect(task).toHaveBeenCalled();
      expect(task.calls.count()).toEqual(1);
    }, 100);
    setTimeout(() => {
      expect(task).toHaveBeenCalled();
      expect(task.calls.count()).toEqual(2);
    }, 200);
    setTimeout(() => {
      expect(task).toHaveBeenCalled();
      expect(task.calls.count()).toEqual(3);
    }, 300);
    setTimeout(() => {
      expect(task).toHaveBeenCalled();
      expect(task.calls.count()).toEqual(4);
    }, 400);
    setTimeout(() => {
      done();
    }, 500);
  });
});
