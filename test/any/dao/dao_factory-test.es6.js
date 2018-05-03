// Copyright 2018 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

describe('RestDAOFactory', () => {
  beforeEach(() => {
    foam.CLASS({
      name: 'Thing',
      package: 'org.chromium.apis.web.test',

      properties: ['id'],
    });
  });

  it('should throw with no URL', () => {
    const factory = org.chromium.apis.web.RestDAOFactory.create();
    expect(() => factory.create({
      of: org.chromium.apis.web.test.Thing,
    })).toThrow();
  });

  it('should produce foam.dao.RestDAO when given URL', () => {
    const factory = org.chromium.apis.web.RestDAOFactory.create({
      baseURL: 'https://example.com/restDAO',
    });
    const restDAO = factory.create({
      of: org.chromium.apis.web.test.Thing,
    });
    expect(restDAO.cls_.id).toBe(foam.dao.RestDAO.id);
  });
})
