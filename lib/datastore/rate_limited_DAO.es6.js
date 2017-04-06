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
        return this.RateLimiter.create();
      },
    },
  ],
  methods: [
    {
      name: 'put',
      documentation: `Enqueue put() to rateLimiter.`,
      code: function(o) {
        this.rateLimiter.enqueue(() => this.delegate.put(o));
      },
    },
  ],
});
