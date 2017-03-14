/**
 * @license
 * Copyright 2017 Google Inc. All Rights Reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the 'License');
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an 'AS IS' BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
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
      documentation: ``,
      required: true,
      final: true,
    },
    {
      class: 'String',
      name: 'osName',
      documentation: `The name of operating system.`,
      required: true,
      final: true,
    },
    {
      class: 'String',
      name: 'osVersion',
      documentation: `The version of operating system.`,
      required: true,
      final: true,
    },
    {
      class: 'String',
      name: 'interfaceName',
      documentation: `The name of an interface. Interfaces in browser
        environment are identified as: first level or __proto__ object
        that have meaningful properties.`,
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
