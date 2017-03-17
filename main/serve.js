// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

require('foam2');

let server = foam.net.node.Server.create({
  port: 9000,
});

server.exportFile('/', `${__dirname}/../static/index.html`);

server.exportDirectory('/', `${__dirname}/../static/bundle`);

server.start();
