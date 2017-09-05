// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

require('angular');

require('../dao_container.es6.js');
require('../web_apis/release.es6.js');
require('../web_apis/release_interface_relationship.es6.js');
require('../web_apis/web_interface.es6.js');
require('./api_confluence.es6.js');
require('./api_matrix.es6.js');
require('./events.es6.js');
const pkg = org.chromium.apis.web;

// TODO(markdittmer): Use foam.lookup() for class lookup. May also need to shove
// this into a FOAM class when porting to FOAM classloader.
angular.module('confluence').service('api', ['$window', function($window) {
  const ctx = foam.box.Context.create({
    myname: '/page',
  });
  const registry = ctx.registry;
  const workerEvents = foam.core.FObject.create();
  const workerEventsBox = foam.box.EventDispatchBox.create({
    target: workerEvents,
  }, ctx);
  registry.register('events', null, workerEventsBox);

  const workerMessagePortBox = foam.box.MessagePortBox.create({
      target: new Worker('worker.bundle.js'),
  }, ctx);

  // TODO(markdittmer): This forces the MessagePort handshake to begin
  // immediately. There should probably be an API for this instead.
  workerMessagePortBox.delegate;

  foam.box.NamedBox.create({
    name: '/worker',
    delegate: workerMessagePortBox,
  }, ctx);

  const C = pkg.DAOContainer;
  const stubFactory = foam.core.StubFactorySingleton.create();
  // Names must match those registered in worker.
  const apiMatrixController = stubFactory.get(pkg.ApiMatrixController).create({
    delegate: foam.box.NamedBox.create({
      name: '/worker/apiMatrixController'
    }, ctx),
  }, ctx);
  const apiConfluence = stubFactory.get(pkg.ApiConfluence).create({
    delegate: foam.box.NamedBox.create({
      name: '/worker/apiConfluence'
    }, ctx),
  }, ctx);

  return {
    apiMatrixController,
    apiConfluence,
    workerEvents: workerEvents,
    promises: [],
  };
}]);
