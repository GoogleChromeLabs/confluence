// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

const path = require('path');

global.FOAM_FLAGS = {gcloud: true};
require('foam2');
require('../lib/datastore/import_controller.es6.js');
require('../lib/datastore/datastore_importer.es6.js');

const ctx = foam.lookup('org.chromium.apis.web.ImportController').create().ctx;
const importer =
    ctx.lookup('org.chromium.apis.web.DatastoreImporter').create({
      apiImporter: ctx.lookup('org.chromium.apis.web.ApiImporter')
          .create(null, ctx),
      objectGraphPath: path.resolve(__dirname, '../data/og/import'),
    }, ctx);

importer.import().then(
    () => ctx.info('Import complete'),
    error => {
      ctx.error('Import failed');
      throw error;
    });
