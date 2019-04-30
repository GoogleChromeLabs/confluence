// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

describe('ApiImporter', function() {
  const webCatalog = {
    'Windows': [
      'Function',
      'property',
    ],
    'Function': [
      'arguments',
      'caller',
    ],
  };

  let JunctionId;
  let apiImporter;
  let mlang;
  let junctionDAO;
  beforeEach(function(done) {
    JunctionId =
      foam.lookup('org.chromium.apis.web.ReleaseWebInterfaceJunctionId');
    const container = global.createDAOContainer();
    apiImporter = foam.lookup('org.chromium.apis.web.ApiImporter')
        .create(null, container);
    mlang = foam.mlang.ExpressionsSingleton.create();
    junctionDAO = container.releaseWebInterfaceJunctionDAO;
    apiImporter.import('Chrome', '56.0.2924.87',
        'OSX', '10.12.2', webCatalog).then(done);
  });

  it('correctly imports releaseWebInterfaceJunction to DAO', function(done) {
    const promises = [
      junctionDAO.find(JunctionId.create({
        sourceId: 'Chrome_56.0.2924.87_OSX_10.12.2',
        targetId: 'Windows#Function',
      })),
      junctionDAO.find(JunctionId.create({
        sourceId: 'Chrome_56.0.2924.87_OSX_10.12.2',
        targetId: 'Windows#property',
      })),
      junctionDAO.find(JunctionId.create({
        sourceId: 'Chrome_56.0.2924.87_OSX_10.12.2',
        targetId: 'Function#arguments',
      })),
      junctionDAO.find(JunctionId.create({
        sourceId: 'Chrome_56.0.2924.87_OSX_10.12.2',
        targetId: 'Function#caller',
      })),
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
