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
  documentation: `BrowserAPI is a FOAM model that contains browser name,
  	browser version, OS, OS version, interface name and API name.`,
  properties: [
    {
      name: 'browserName',
      required: true,
      final: true,
      class: 'String',
    },
    {
      name: 'browserVersion',
      required: true,
      final: true,
      class: 'String',
    },
    {
      name: 'osName',
      required: true,
      final: true,
      class: 'String',
    },
    {
      name: 'osVersion',
      required: true,
      final: true,
      class: 'String',
    },
    {
      name: 'interfaceName',
      required: true,
      final: true,
      class: 'String',
    },
    {
      name: 'apiName',
      required: true,
      final: true,
      class: 'String',
    },
    {
      name: 'browserKey',
      documentation: `An unique key for this browser.`,
      class: 'String',
      expression: function(browserName, browserVersion, osName, osVersion) {
        return`${browserName}_${browserVersion}_${osName}_${osVersion}`;
      },
    },
    {
      name: 'interfaceKey',
      documentation: `A key for this pair of interface and API.`,
      class: 'String',
      expression: function(interfaceName, apiName) {
        return`${interfaceName}_${apiName}`;
      },
    },
  ],
  ids: [
    'browserName',
    'browserVersion',
    'osName',
    'osVersion',
    'interfaceName',
    'apiName',
  ],
});
