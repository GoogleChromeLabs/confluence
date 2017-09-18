// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

require('./handler.es6.js');

foam.CLASS({
  package: 'org.chromium.apis.web',
  name: 'ClockCache',

  documentation: `An implementation of clock page replacement algorithm [1], an
      approximation of an LRU cache.

      [1] https://en.wikipedia.org/wiki/Page_replacement_algorithm#Clock`,

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
      documentation: 'Number of elements in cache array.',
      name: 'size',
    },
    {
      class: 'Int',
      documentation: `Array index that is next for "mark for removal, or
          remove".`,
      name: 'clockIdx_',
    },
    {
      class: 'Array',
      documentation: 'Array of Item objects that constitute cache.',
      name: 'data_',
    },
    {
      class: 'Int',
      documentation: 'Number of items actually stored in cache.',
      name: 'numItems_',
    },
  ],

  methods: [
    function init() {
      this.SUPER();
      this.data_ = new Array(this.size);
    },
    function isFull() { return this.numItems_ === this.size; },
    function store(data) {
      // Update existing Item, if found.
      let idx = this.getIdx_(data.id);
      if (idx !== -1) {
        this.data_[idx].data = data;
        this.data_[idx].markedForRemoval = false;
        return;
      }

      // Store new Item.
      if (this.isFull()) {
        this.evictOne_();
        idx = this.clockIdx_;
        this.clockIdx_++;
      } else {
        idx = this.numItems_;
        this.numItems_++;
        this.data_[idx] = this.data_[idx] || this.Item.create();
      }
      this.data_[idx].data = data;
    },
    function find(id) {
      const idx = this.getIdx_(id);
      if (idx === -1) return null;
      this.data_[idx].markedForRemoval = false;
      return this.data_[idx].data;
    },
    function clear() {
      const array = this.data_;
      const limit = this.isFull() ? array.length : this.numItems_;
      for (let i = 0; i < limit; i++) {
        array[i].data = undefined;
        array[i].markedForRemoval = false;
      }
      this.clockIdx_ = this.numItems_ = 0;
    },
    function getIdx_(id) {
      const array = this.data_;
      const limit = this.numItems_;
      for (let i = 0; i < limit; i++) {
        if (array[i].data.id === id) return i;
      }
      return -1;
    },
    function evictOne_() {
      const mod = this.size;
      const array = this.data_;
      let i;
      for (i = this.clockIdx_; !array[i].markedForRemoval; i = i + 1 % mod)
        array[i].markedForRemoval = true;
      this.clockIdx_ = i;
      array[i].data = undefined;
      array[i].markedForRemoval = false;
    },
  ],
});

foam.CLASS({
  package: 'org.chromium.apis.web',
  name: 'CacheHandler',
  extends: 'org.chromium.apis.web.Handler',

  documentation: 'Cache responses by replaying them until clearCache().',

  requires: ['org.chromium.apis.web.ClockCache'],

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
      class: 'Proxy',
      of: 'org.chromium.apis.web.Handler',
      name: 'delegate',
      documentation: 'Delegate handler over which to cache responses.',
      required: true,
    },
    {
      class: 'Int',
      name: 'cacheSize',
      value: 50,
    },
    {
      class: 'FObjectProperty',
      of: 'org.chromium.apis.web.ClockCache',
      name: 'cache_',
      factory: function() {
        return this.ClockCache.create({
          size: this.cacheSize,
        });
      },
    },
  ],

  methods: [
    function clearCache() { this.cache_.clear(); },
    function handleRequest(req, res) {
      let precache = {
        req: req,
        // Incomplete RecordedResponse.id.
        id: `${req.method} ${req.url} HTTP/${req.httpVersion}\n`,
      };

      const onDataRead = this.onDataRead.bind(this, precache);
      const onReadEnd = this.onReadEnd.bind(this, precache, res);
      req.on('data', onDataRead);
      req.on('end', onReadEnd);
    },
  ],

  listeners: [
    function onDataRead(precache, buffer) { precache.id += buffer.toString(); },
    function onReadEnd(precache, res) {
      const id = precache.id;
      const cachedRecordedRes = this.cache_.find(id);
      if (cachedRecordedRes) {
        cachedRecordedRes.replay(res);
        return;
      }

      const recordedRes = this.RecordedResponse.create({id});
      recordedRes.ended.sub(this.onEnded.bind(this, recordedRes, res));
      this.delegate.handleRequest(precache.req, recordedRes);
      this.cache_.store(recordedRes);
    },
    function onEnded(recordedRes, res) {
      recordedRes.replay(res);
    },
  ],
});
