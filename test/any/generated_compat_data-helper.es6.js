// Copyright 2018 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

global.defineGeneratedCompatData = (gen, releaseSpecs) => {
  // Register custom CompatData before looking up classes and instantiating
  // instances.
  foam.CLASS({
    name: 'CompatData',
    package: 'org.chromium.apis.web.test',
    extends: 'org.chromium.apis.web.AbstractApiCompatData',

    properties: releaseSpecs.map(r => {
      return {
        class: 'org.chromium.apis.web.CompatProperty',
        release: r,
        name: gen.propertyNameFromRelease(r),
        label: gen.propertyLabelFromRelease(r),
      };
    }),
  });
  foam.register(org.chromium.apis.web.test.CompatData,
      'org.chromium.apis.web.generated.CompatData');
  return foam.lookup('org.chromium.apis.web.generated.CompatData');
};
