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

describe('Browser', function() {
  describe('browser id', function() {
    it('returns same id for two identitical browsers', function() {
      let chromeA = com.web.api.Browser.create({
        browserName: 'Chrome',
        browserVersion: '56',
        os: 'Windows',
        osVersion: '10',
      }).id;
      let chromeB = com.web.api.Browser.create({
        browserName: 'Chrome',
        browserVersion: '56',
        os: 'Windows',
        osVersion: '10',
      }).id;
      expect(chromeA).toBe(chromeB);

      let firefoxA = com.web.api.Browser.create({
        browserName: 'Firefox',
        browserVersion: '50',
        os: 'OSX',
        osVersion: '10.11',
      }).id;
      let firefoxB = com.web.api.Browser.create({
        browserName: 'Firefox',
        browserVersion: '50',
        os: 'OSX',
        osVersion: '10.11',
      }).id;
      expect(firefoxA).toBe(firefoxB);
    });
  });
  describe('getBrowserKey', function() {
    it('returns the correct key for any browsers', function() {
      let chrome57Key = com.web.api.Browser.create({
        browserName: 'Chrome',
        browserVersion: '57',
        os: 'OSX',
        osVersion: '10.11',
      }).getBrowserKey();
      expect(chrome57Key).toBe('Chrome_57_OSX_10.11');

      let Safari602Key = com.web.api.Browser.create({
        browserName: 'Safari',
        browserVersion: '602.4.8',
        os: 'OSX',
        osVersion: '10.12.3',
      }).getBrowserKey();
      expect(Safari602Key).toBe('Safari_602.4.8_OSX_10.12.3');

      let Edge12Key = com.web.api.Browser.create({
        browserName: 'Edge',
        browserVersion: '12',
        os: 'Windows',
        osVersion: '10',
      }).getBrowserKey();
      expect(Edge12Key).toBe('Edge_12_Windows_10');
    });
  });
});
