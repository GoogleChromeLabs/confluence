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
  name: 'Browser_StrId',
  package: 'com.web.api',
  properties: [
    {
      name: 'browserName',
      required: true,
    },
    {
      name: 'browserVersion',
      required: true,
    },
    {
      name: 'os',
      required: true,
    },
    {
      name: 'osVersion',
      required: true,
    },
  ],
  ids: ['browserName', 'browserVersion', 'os', 'osVersion'],
  methods: [
    {
      name: 'getBrowserKey',
      documentation: `Get the key of this browser, including browser name,
        browser version, OS and OS version.`,
      returns: {
        documentation: `The key of this browser.`,
        typeName: `String`,
      },
      code: function() {
        return [this.browserName, this.browserVersion, this.os,
          this.osVersion].join('_');
      },
    },
  ],
});
