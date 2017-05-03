// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

foam.CLASS({
  package: 'org.chromium.mlang.sink',
  name: 'SetMinus',
  extends: 'foam.dao.ProxySink',

  documentation: `Put incoming objects to delegate iff the same object (by id)
      is not found in "subtrahend" DAO.`,

  properties: [
    {
      class: 'foam.dao.DAOProperty',
      documentation: `"subtrahend" in set operation:
          SET_MINUS(minuend, subtrahend)`,
      name: 'subtrahend',
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
    },
    {
      documentation: `Make SetMinus thenable so that
          dao.select(SET_MINUS(...)).then(next) invokes "next" after all
          put()s have been dropped or forwarded.`,
      name: 'then',
      code: function then(resolve, reject) {
        return this.promise_.then(resolve, reject);
      },
    },
  ],

  listeners: [
    function onNext(sub, o) {
      return this.subtrahend.find(o).then(
          this.onSubtrahendFind.bind(this, sub, o),
          this.onError);
    },
    function onSubtrahendFind(sub, o, found) {
      if (found === null) this.delegate.put(sub, o);
    },
    function onError(error) {
      this.rejected_ = true;
      this.reject_(error);
    },
  ],
});

foam.CLASS({
  package: 'org.chromium.mlang',
  name: 'Expressions',
  refines: 'foam.mlang.Expressions',

  documentation: 'Adds set operations to foam.mlang.Expressions',

  requires: [
    'org.chromium.mlang.sink.SetMinus',
  ],

  methods: [
    function SET_MINUS(dao, sink) {
      return this.SetMinus.create({subtrahend: dao, delegate: sink});
    },
  ],
});
