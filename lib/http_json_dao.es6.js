// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

foam.CLASS({
  package: 'org.chromium.apis.web',
  name: 'HttpJsonDAO',
  extends: 'foam.dao.PromisedDAO',

  documentation: `DAO that fetches a JSON array via HTTP.`,

  requires: [
    'foam.box.ClassWhitelistContext',
    'foam.dao.ArrayDAO',
    'foam.json.Parser',
    'foam.net.HTTPRequest',
  ],

  constants: {
    SAFE_URL_PROTOCOL: 'https:',
    SAFE_URL_HOSTNAME: 'storage.googleapis.com',
    SAFE_URL_PATHNAME_PREFIX: '/web-api-confluence-data-cache/',
    ERROR_URL: 'https://storage.googleapis.com/web-api-confluence-data-cache/does-not-exist',
  },

  properties: [
    {
      class: 'String',
      name: 'url',
      documentation: 'Location of file containing JSON array.',
      preSet: function(old, nu) {
        // Thwart attempts to set URL to something unsafe. I.e., only trust code
        // from URLs matching "SAFE_*" constants.
        if (!foam.String.isInstance(nu)) return this.ERROR_URL;
        const url = this.createURL(nu);
        if (url.hostname === 'localhost') return nu;
        if (url.protocol !== this.SAFE_URL_PROTOCOL ||
            url.hostname !== this.SAFE_URL_HOSTNAME ||
            url.pathname.indexOf(this.SAFE_URL_PATHNAME_PREFIX) !== 0) {
          return this.ERROR_URL;
        }
        return nu;
      },
      required: true,
    },
    {
      class: 'FObjectProperty',
      of: 'foam.json.Parser',
      name: 'parser',
      factory: function() {
        // NOTE: Configuration must be consistent with outputters in
        // corresponding foam.dao.RestDAO.
        return this.Parser.create({
          strict: true,
          creationContext: this.ClassWhitelistContext.create({
            whitelist: require('../data/class_whitelist.json'),
          }, this).__subContext__,
        });
      },
    },
    {
      class: 'Function',
      name: 'createURL',
      documentation: 'Platform-independent URL factory function.',
      factory: function() {
        return foam.isServer ?
            str => require('url').parse(str) :
            str => new URL(str);
      },
    },
    {
      name: 'promise',
      factory: function() {
        this.validate();
        const url = this.url;
        return this.HTTPRequest.create({
          url,
          responseType: 'text',
        }).send().then(response => {
          if (response.status !== 200) throw response;
          return response.payload;
        }).then(jsonStr => {
          const array = this.parser.parseString(jsonStr,
                                                this.parser.creationContext);
          foam.assert(Array.isArray(array), 'HttpJsonDAO: Expected array');
          return this.ArrayDAO.create({
            of: this.of,
            array: array,
          });
        });
      },
    },
  ],
});

foam.CLASS({
  package: 'org.chromium.apis.web',
  name: 'SerializableHttpJsonDAO',
  extends: 'foam.dao.ProxyDAO',

  documentation: `A proxy that can be instantiated as a HttpJsonDAO
      configuration (without fetching data) in one context, and then deployed
      (by fetching data) in another. This is achieved by using a lazy factory
      on a transient "delegate" property; only in contexts where the "delegate"
      property is accessed will fetch data.`,

  requires: ['org.chromium.apis.web.HttpJsonDAO'],

  properties: [
    {
      class: 'String',
      name: 'url',
      documentation: 'Configuration passed to delegate.',
      required: true,
    },
    {
      class: 'foam.dao.DAOProperty',
      name: 'delegate',
      documentation: 'Transient DAO that will fetch data when instantiated.',
      transient: true,
      factory: function() {
        this.validate();
        return this.HttpJsonDAO.create({
          url: this.url,
        });
      },
    },
  ],
});
