// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

describe('ApiImporter', function() {
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

  let apiImporter;
  let mlang;
  let junctionDAO;
  beforeEach(function(done) {
    let container = global.createDAOContainer();
    apiImporter = foam.lookup('org.chromium.apis.web.ApiImporter')
        .create(null, container);
    mlang = foam.mlang.ExpressionsSingleton.create();
    junctionDAO = container.releaseWebInterfaceJunctionDAO;
    apiImporter.import('Chrome', '56.0.2924.87',
                       'OSX', '10.12.2', webCatalog).then(done);
  });

  it('correctly imports releaseWebInterfaceJunction to DAO', function(done) {
    let promises = [
      junctionDAO.find([
        'Chrome_56.0.2924.87_OSX_10.12.2',
        'Windows#Function',
      ]),
      junctionDAO.find([
        'Chrome_56.0.2924.87_OSX_10.12.2',
        'Windows#property',
      ]),
      junctionDAO.find([
        'Chrome_56.0.2924.87_OSX_10.12.2',
        'Function#arguments',
      ]),
      junctionDAO.find([
        'Chrome_56.0.2924.87_OSX_10.12.2',
        'Function#caller',
      ]),
    ];
    Promise.all(promises).then((results) => {
      results.forEach((releaseAPI) => {
        expect(releaseAPI).not.toBeNull();
      });
      return junctionDAO.select(mlang.COUNT());
    }).then((countSink) => {
      expect(countSink.value).toBe(promises.length);
      done();
    });
  });
});
