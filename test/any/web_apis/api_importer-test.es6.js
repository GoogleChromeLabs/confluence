// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

describe('ApiImporter', function() {
  let apiImporter = org.chromium.apis.web.ApiImporter.create();
  let mlang = foam.mlang.ExpressionsSingleton.create();
  let webCatalog = {
    'Windows': [
      'Function',
      'property',
    ],
    'Function': [
      'arguments',
      'caller',
    ],
  };

  apiImporter.import('Chrome', '56.0.2924.87',
    'OSX', '10.12.2', webCatalog);
  it('correctly imports browserWebInterfaceJunction to DAO', function(done) {
    let promises = [
      apiImporter.browserApiDAO.find([
        'Chrome_56.0.2924.87_OSX_10.12.2',
        'Windows#Function',
      ]),
      apiImporter.browserApiDAO.find([
        'Chrome_56.0.2924.87_OSX_10.12.2',
        'Windows#property',
      ]),
      apiImporter.browserApiDAO.find([
        'Chrome_56.0.2924.87_OSX_10.12.2',
        'Function#arguments',
      ]),
      apiImporter.browserApiDAO.find([
        'Chrome_56.0.2924.87_OSX_10.12.2',
        'Function#caller',
      ]),
    ];
    Promise.all(promises).then((results) => {
      results.forEach((browserAPI) => {
        expect(browserAPI).not.toBeNull();
      });
      return apiImporter.browserApiDAO.select(mlang.COUNT());
    }).then((countSink) => {
      expect(countSink.value).toBe(promises.length);
      done();
    });
  });
});
