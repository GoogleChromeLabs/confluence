// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

foam.CLASS({
  name: 'BrowserAPI',
  package: 'org.chromium.apis.web',
  documentation: `BrowserAPI that contains browser name, browser version,
  	 OS, OS version, interface name and API name.`,
  ids: [
    'browserName',
    'browserVersion',
    'osName',
    'osVersion',
    'interfaceName',
    'apiName',
  ],
  properties: [
    {
      class: 'String',
      name: 'browserName',
      documentation: `The name of browser.`,
      required: true,
      final: true,
    },
    {
      class: 'String',
      name: 'browserVersion',
      documentation: `The version of browser.`,
      required: true,
      final: true,
    },
    {
      class: 'String',
      name: 'osName',
      documentation: `The name of operating system as reported by the
        object-graph-js library.`,
      required: true,
      final: true,
    },
    {
      class: 'String',
      name: 'osVersion',
      documentation: `The version of operating system as reported by the
        object-graph-js library.`,
      required: true,
      final: true,
    },
    {
      class: 'String',
      name: 'interfaceName',
      documentation: `The name of a web interface, extracted from
        apiExtractor.`,
      required: true,
      final: true,
    },
    {
      class: 'String',
      name: 'apiName',
      documentation: `The name of an API. APIs are identified as meaningful
        properties of an interface.`,
      required: true,
      final: true,
    },
    {
      class: 'String',
      name: 'browserKey',
      documentation: `An unique key for this browser. Avoid the need for
        CONCAT mLang or similar to be able to groupBy browserName,
        browserVersion, osName, osVersion.`,
      expression: function(browserName, browserVersion, osName, osVersion) {
        return`${browserName}_${browserVersion}_${osName}_${osVersion}`;
      },
    },
    {
      class: 'String',
      name: 'interfaceKey',
      documentation: `A key for this pair of interface and API. Avoid the
        need for CONCAT mlang or similar to be able to groupBy interfaceName,
        apiName.`,
      expression: function(interfaceName, apiName) {
        return`${interfaceName}_${apiName}`;
      },
    },
  ],
});
