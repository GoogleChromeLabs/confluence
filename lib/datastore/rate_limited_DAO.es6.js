// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

require('./rate_limiter.es6.js');

foam.CLASS({
  package: 'org.chromium.apis.web',
  name: 'RateLimitedDAO',
  extends: 'foam.dao.ProxyDAO',
  documentation: `A RateLimiterDAO limits the rate of put()s its delegate.`,
  requires: [
    'org.chromium.apis.web.RateLimiter',
  ],
  properties: [
    {
      name: 'rateLimiter',
      documentation: `RateLimiter calls enqueued tasks at specified intervals`,
      factory: function() {
        return this.RateLimiter.create({interval: this.interval});
      },
    },
    {
      class: 'Int',
      name: 'interval',
      documentation: 'The time interval between each put operation.',
      value: 30,
    },
  ],
  methods: [
    {
      name: 'put',
      documentation: `Enqueue put() to rateLimiter.`,
      code: function(o) {
        return this.rateLimiter.enqueue(() => this.delegate.put(o));
      },
    },
  ],
});
