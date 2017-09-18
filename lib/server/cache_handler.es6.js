// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

foam.CLASS({
  package: 'org.chromium.apis.web',
  name: 'LRUClockCache',

  classes: [
    {
      name: 'Item',

      properties: [
        {
          class: 'Boolean',
          name: 'markedForRemoval'
        },
        {
          name: 'data'
        }
      ],
    },
  ],

  properties: [
    {
      class: 'Int',
      name: 'size',
    },
    {
      class: 'Int',
      name: 'clockIdx_',
    },
    {
      class: 'Array',
      name: 'data_',
    },
    {
      class: 'Int',
      name: 'numItems_',
    },
  ],

  methods: [
    function init() {
      this.SUPER();
      this.data_ = new Array(this.size);
    },
    function isFull() { return this.numItems_ === this.size; },
    function push(data) {
      let idx;
      if (this.isFull()) {
        this.evictOne_();
        idx = this.clockIdx_;
        this.clockIdx_++;
      } else {
        idx = this.numItems_;
        this.numItems_++;
      }
      this.data_[idx] = this.Item.create({data});
    },
    function find() {
      // NEXT TODO(markdittmer): Implement find-by-id or find-by-predicate
      // function, then use LRUCache in CacheHandler.
    },
    function evictOne_() {
      const mod = this.size;
      let i;
      for (i = this.clockIdx_;
           !this.data_[i].markedForRemoval;
           i = i + 1 % mod)
        this.data_[i].markedForRemoval = true;
      this.clockIdx_ = i;
      this.data_[i] = undefined;
    },
  ],
});

// TODO(markdittmer): This strategy does not cancel work initiated by
// handle(). Such a strategy would require decoupling "I will handle this
// request" signal (currently return value of handle()) and "run the request
// handler" (also baked in to handle()).

foam.CLASS({
  package: 'org.chromium.apis.web',
  name: 'CacheHandler',
  extends: 'foam.net.node.ProxyHandler',

  documentation: 'Cache responses by replaying them until clearCache().',


  classes: [
    {
      name: 'RecordedResponse',

      documentation: `Fake a subset of NodeJS http.ServerResponse API. Store
          an array of functions to replay http.ServerResponse operations.`,

      topics: ['ended'], // Required to trigger replay on uncached requests.

      properties: [
        {
          class: 'String',
          name: 'id'
        },
        {
          class: 'Int',
          name: 'statusCode',
          documentation: `Included to capture http.ServerResponse.statusCode
              setter`,
          value: 200,
          postSet: function(old, nu) {
            this.calls_.push(function(res) { res.responseCode = nu; });
          }
        },
        {
          class: 'Array',
          of: 'Function',
          name: 'calls_',
          documentation: `Array of functions that replay http.ServerResponse
              operations.`
        },
      ],

      methods: [
        //
        // Custom replay interface.
        //

        function replay(res) {
          const calls = this.calls_;
          for (let i = 0; i < calls.length; i++) {
            this.calls_[i](res);
          }
        },

        //
        // http.ServerResponse operations.
        //

        function setHeader() {
          const args = arguments;
          this.calls_.push(function(res) { res.setHeader.apply(res, args); });
        },
        function write() {
          const args = arguments;
          this.calls_.push(function(res) { res.write.apply(res, args); });
        },
        function end() {
          const args = arguments;
          this.calls_.push(function(res) { res.end.apply(res, args); });
          this.ended.pub();
        },
      ],
    },
  ],

  properties: [
    {
      name: 'delegate',
      documentation: 'Delegate handler over which to cache responses.',
      required: true,
    },
    {
      class: 'Array',
      // of: 'RecordedResponse',
      name: 'cache_',
    },
  ],

  methods: [
    function clearCache() { this.cache_ = []; },
    function handle(req, res) {
      const recordedRes = this.RecordedResponse.create();

      let precache = [
        // Incomplete RecordedResponse.id.
        `${req.method} ${req.url} HTTP/${req.httpVersion}\n`,
        // RecordedResponse to bind id to.
        recordedRes,
        // Signal to unconditionally replay on final callback.
        false,
      ];
      recordedRes.ended.sub(this.onEnded.bind(this, precache, res));

      const onDataRead = this.onDataRead.bind(this, precache);
      const onReadEnd = this.onReadEnd.bind(this, precache, res);
      req.on('data', onDataRead);
      req.on('end', onReadEnd);

      const result = this.delegate.handle(req, recordedRes);

      if (!result) {
        req.removeListener('data', onDataRead);
        req.removeListener('end', onReadEnd);
      }

      return result;
    },
  ],

  listeners: [
    function onDataRead(precache, buffer) {
      precache[0] += buffer.toString();
    },
    function onReadEnd(precache, res) {
      const id = precache[0];
      const cache = this.cache_;
      for (var i = 0; i < cache.length; i++) {
        if (cache[i].id === id) {
          cache[i].replay(res);
          return;
        }
      }

      let recordedRes = precache[1];
      const needsReplay = precache[2];
      recordedRes.id = id;
      this.cache_.push(recordedRes);

      // If onEnded signaled replay, then replay now.
      if (needsReplay) recordedRes.replay(res);
      else precache[2] = true;
    },
    function onEnded(precache, res) {
      let recordedRes = precache[1];
      const needsReplay = precache[2];

      // If onReadEnd signaled replay, then replay now.
      if (needsReplay) recordedRes.replay(res);
      else precache[2] = true;
    },
  ],
});
