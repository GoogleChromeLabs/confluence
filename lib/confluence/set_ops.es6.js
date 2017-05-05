// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

foam.INTERFACE({
  package: 'org.chromium.mlang.sink',
  name: 'AsyncSinkPutDecider',

  documentation: `Decides whether or not an AsycSink should forward an object
      from primary DAO, based on the result of attempting to find() the object
      in the secondary DAO.`,

  methods: [function shouldPutToDelegate(original, found) {}]
});

foam.CLASS({
  package: 'org.chromium.mlang.sink',
  name: 'AsyncSink',
  extends: 'foam.dao.ProxySink',

  documentation: `Common pattern for proxy sinks that wait on async operations.
      This abstract class takes care control flow on put() as follows:
      (1) Perform find() on "secondary" DAO;
      (2) Delegate to "decider" to decide whether or not to put() to delegate.

      Additionally, this class manages:
      (1) Halting subsequent put() operations when an error occurs;
      (2) Implementing the sink as a thenable that resolves after all
          put()/error()/eof() operations are complete.`,

  properties: [
    {
      class: 'foam.dao.DAOProperty',
      documentation: `"secondary" in binary set operation:
          OPERATION(primary, secondary)`,
      name: 'secondary',
      required: true,
    },
    {
      class: 'FObjectProperty',
      of: 'org.chromium.mlang.sink.AsyncSinkPutDecider',
      name: 'decider',
      required: true,
    },
    {
      name: 'next_',
      documentation: `Next Promise to wait for before committing an incoming
          put().`,
      factory: () => Promise.resolve(),
    },
    {
      documentation: `Promise to be resolved when all put()s have been dropped
          or forwarded.`,
      name: 'promise_',
    },
    {
      class: 'Function',
      documentation: 'Resolve function associated with "promise_".',
      name: 'resolve_',
    },
    {
      class: 'Function',
      documentation: 'Reject function associated with "promise_".',
      name: 'reject_',
    },
    {
      class: 'Boolean',
      documentation: "Indicator: Has this sink's select() already rejected?",
      name: 'rejected_',
    },
  ],

  methods: [
    function init() {
      // Initialize callbacks for when sink is finished.
      this.promise_ = new Promise((resolve, reject) => {
        this.resolve_ = resolve;
        this.reject_ = reject;
      });
    },
    function put(sub, o) {
      if (this.rejected_) return;
      this.next_ = this.next_.then(
          this.onNext.bind(this, sub, o),
          this.onError);
    },
    function eof() {
      if (this.rejected_) return;
      // Call resolve() after in-flight put()s resolve.
      this.next_.then(this.resolve_.bind(this, this.delegate), this.onError);
      this.delegate.eof();
    },
    {
      documentation: `Make sink thenable so that
          dao.select(CUSTOM_SINK(...)).then(next) invokes "next" after all
          put()s have been dropped or forwarded.`,
      name: 'then',
      code: function then(resolve, reject) {
        return this.promise_.then(resolve, reject);
      },
    },
  ],

  listeners: [
    function onNext(sub, o) {
      return this.secondary.find(o).then(
          // Delegate to concrete class's onSecondaryFind() implementation to
          // decide what to do with object that has been put() to this sink.
          this.onSecondaryFind.bind(this, sub, o),
          this.onError);
    },
    function onSecondaryFind(sub, original, found) {
      if (this.decider.shouldPutToDelegate(original, found))
        this.delegate.put(sub, original);
    },
    function onError(error) {
      this.rejected_ = true;
      this.reject_(error);
    },
  ],
});

foam.CLASS({
  package: 'org.chromium.mlang.sink',
  name: 'SetMinusDecider',
  implements: ['org.chromium.mlang.sink.AsyncSinkPutDecider'],
  axioms: [
    foam.pattern.Singleton.create(),
  ],

  documentation: `Put incoming objects to delegate iff the same object (by id)
      is not found in "secondary" (subtrahend) DAO.`,

  methods: [
    function shouldPutToDelegate(original, found) {
      return found === null;
    },
  ],
});

foam.CLASS({
  package: 'org.chromium.mlang.sink',
  name: 'IntersectDecider',
  implements: ['org.chromium.mlang.sink.AsyncSinkPutDecider'],
  axioms: [
    foam.pattern.Singleton.create(),
  ],

  documentation: `Put incoming objects to delegate iff the same object (by id)
      is found in "secondary" DAO.`,

  methods: [
    function shouldPutToDelegate(original, found) {
      return found !== null;
    },
  ],
});

foam.CLASS({
  package: 'org.chromium.dao',
  name: 'UnionDAO',
  extends: 'foam.dao.AbstractDAO',
  implements: ['foam.mlang.Expressions'],

  documentation: 'Read-only DAO that captures the union of two delegate DAOs.',

  requires: ['foam.dao.ArraySink'],

  classes: [
    {
      name: 'NoEOFSink',
      extends: 'foam.dao.ProxySink',

      methods: [function eof() {}],
    },
  ],

  properties: [
    {
      class: 'foam.dao.DAOProperty',
      name: 'primary',
      required: true,
    },
    {
      class: 'foam.dao.DAOProperty',
      name: 'secondary',
      required: true,
    },
    {
      name: 'of',
      factory: function() {
        return this.primary.of;
      },
    },
  ],

  methods: [
    function put(o) {
      throw new Error('Attempt to put() to read-only UnionDAO');
    },
    function remove(o) {
      throw new Error('Attempt to remove() from read-only UnionDAO');
    },
    function removeAll(o) {
      throw new Error('Attempt to removeAll() from read-only UnionDAO');
    },
    function find(key) {
      return new Promise((resolve, reject) => {
        var data = {done: false, resolve: resolve, reject: reject};
        var onFind = this.onFind.bind(this, data);
        var onError = this.onFindError.bind(this, data);
        this.primary.find(key).then(onFind, onError);
        this.secondary.find(key).then(onFind, onError);
      });
    },
    function select(opt_sink, skip, limit, order, predicate) {
      var sink = opt_sink || this.ArraySink.create();
      if (skip || limit || order || predicate)
        throw new Error('UnionDAO does not support skip/limit/order/predicate');

      return new Promise((resolve, reject) => {
        var data = {doneCount: 0, errorCount: 0, sink: sink, resolve: resolve, reject: reject};
        var onSelect = this.onSelect.bind(this, data);
        var onError = this.onSelectError.bind(this, data);

        // Select primary -> "sink"-with-no-eof() callback.
        this.primary.select(this.NoEOFSink.create({delegate: sink}))
            .then(onSelect, onError);
        // Select secondary \ primary -> "sink"-with-no-eof() callback.
        this.secondary.select(
            this.SET_MINUS(this.primary, this.NoEOFSink.create({
              delegate: sink,
            })))
            .then(onSelect, onError);
      });
    },
    function listen(sink, skip, limit, order, predicate) {
      if (skip || limit || order || predicate)
        throw new Error('UnionDAO does not support skip/limit/order/predicate');

      // Listen to primary.
      this.primary.listen(sink);
      // Listen to secondary \ primary.
      this.secondary.listen(this.SET_MINUS(this.primary, sink));
    },
  ],

  listeners: [
    function onFind(data, o) {
      if (data.done) return;
      data.done = true;
      data.resolve(o);
    },
    function onFindError(data, error) {
      if (data.done) return;
      data.done = true;
      data.reject(error);
    },
    function onSelect(data, o) {
      // Halt select() control flow after first error.
      if (data.errorCount !== 0) return;

      // Do not eof() + resolve() until both select() control flows complete.
      data.doneCount++;
      if (data.doneCount !== 2) return;

      // Ensure exactly one eof() call after all inner select()s complete.
      data.sink.eof();
      data.resolve(data.sink);
    },
    function onSelectError(data, error) {
      // Halt select() control flow after first error.
      if (data.errorCount !== 0) return;

      data.errorCount++;
      data.reject(error);
    },
  ],
});

foam.CLASS({
  package: 'org.chromium.mlang',
  name: 'Expressions',
  refines: 'foam.mlang.Expressions',

  documentation: 'Adds set operations to foam.mlang.Expressions',

  requires: [
    'org.chromium.dao.UnionDAO',
    'org.chromium.mlang.sink.AsyncSink',
    'org.chromium.mlang.sink.IntersectDecider',
    'org.chromium.mlang.sink.SetMinusDecider',
  ],

  methods: [
    function SET_MINUS(dao, sink) {
      return this.AsyncSink.create({
        decider: this.SetMinusDecider.create(),
        delegate: sink,
        secondary: dao,
      });
    },
    function INTERSECT(dao, sink) {
      return this.AsyncSink.create({
        decider: this.IntersectDecider.create(),
        delegate: sink,
        secondary: dao,
      });
    },
    function UNION(primary, secondary) {
      return this.UnionDAO.create({
        primary: primary,
        secondary: secondary,
      });
    },
  ],
});
