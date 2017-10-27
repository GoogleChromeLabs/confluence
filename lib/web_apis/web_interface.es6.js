// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

foam.CLASS({
  name: 'WebInterface',
  package: 'org.chromium.apis.web',
  ids: ['interfaceKey'],

  // TODO(markdittmer): Really, this is a static method. Upgrade when
  // https://github.com/foam-framework/foam2/issues/613 is fixed.
  constants: {
    // Provide as many property values as possible from id encoding.
    PROPERTIES_FROM_ID: function(id) {
      const interfaceKey = id;
      const apiData = interfaceKey.split('#');
      foam.assert(
          apiData.length === 2,
          'WebInterface.interfaceKey: Expected <interface-name>#<apiName>');
      return {
        interfaceKey,
        interfaceName: apiData[0],
        apiName: apiData[1],
      };
    }
  },

  properties: [
    {
      class: 'String',
      name: 'interfaceName',
      documentation: `The name of a web interface, extracted from
        ApiExtractor.`,
      required: true,
      final: true,
    },
    {
      class: 'String',
      name: 'apiName',
      // TODO: add public documentation on how we define and gather "meaningful"
      // properties.
      documentation: `The name of an API. APIs are identified as meaningful
        properties of an interface.`,
      required: true,
      final: true,
    },
    {
      class: 'String',
      name: 'interfaceKey',
      documentation: `An unique key for this pair of interface/API.
      	Avoid the need for CONCAT mLang or similar to be able to
      	groupBy interfaceName, apiName.`,
      expression: function(interfaceName, apiName) {
        return`${interfaceName}#${apiName}`;
      },
      final: true,
    },
  ],
});
