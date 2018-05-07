// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

self.window = self.global = self;
importScripts('vendors.bundle.js', 'foam.bundle.js');

require('../lib/compat.es6.js');
require('../lib/dao_container.es6.js');
require('../lib/web_apis/api_compat_data.es6.js');
require('../lib/worker_dao.es6.js');
const pkg = org.chromium.apis.web;

// Has initial "this is your name" message been received?
let nameReceived = false;
// Post-"this is your name" event backlog; worker context setup is async.
let events = [];
self.onmessage =  e => {
  // If name already received, queue event in backlog.
  if (nameReceived) {
    events.push(e);
    return;
  }

  nameReceived = true;

  foam.assert(
      e.data && e.data.name, 'Failed to recieve name from worker owner');
  const name = e.data.name;

  // Set up worker context.
  //
  // TODO(markdittmer): Code generation is really only necessary  for worker(s)
  // that deal with generated.CompatData model. Should this be somehow
  // conditional and/or driven by a signal from the worker's owner?
  const compatClassURL = `${window.location.origin}/${pkg.DAOContainer.COMPAT_MODEL_FILE_NAME}`;
  pkg.ClassGenerator.create({
    classURL: compatClassURL,
  }).generateClass().then(() => {
    const ctx = foam.box.Context.create({
      myname: name,
      unsafe: false,
      classWhitelist: require('../data/class_whitelist.json'),
    }, pkg.DAOContainer.create());

    // Replay or wait for message port connection from owner.
    const portHandler = e => {
      if (!e.data instanceof MessagePort) {
        throw new Error('Unexpected control message', e.data);
      }

      // Sets self.onmessage.
      ctx.messagePortService.addPort(e.data);
    };
    if (events.length > 0) {
      portHandler(events[0]);
      for (let i = 1; i < events.length; i++) {
        self.onmessage(events[i]);
      }
    } else {
      self.onmessage = portHandler;
    }
  });
};

// Assert that initial "this is your name" message is recieved in a timely
// manner.
self.setTimeout(
    () => foam.assert(
        nameReceived, 'Worker timed out waiting for name from owner'),
    1000);
