// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

require('./indexed_dao.es6.js');

foam.CLASS({
  package: 'org.chromium.apis.web',
  name: 'HttpJsonDAO',
  extends: 'foam.dao.PromisedDAO',
  implements: ['org.chromium.apis.web.AbstractFacetedIndexed'],

  documentation: `DAO that fetches a JSON array via HTTP.`,

  requires: [
    'foam.box.ClassWhitelistContext',
    'foam.json.Parser',
    'foam.net.HTTPRequest',
    'org.chromium.apis.web.MDAO',
  ],

  constants: {
    ERROR_URL: 'https://storage.googleapis.com/web-api-confluence-data-cache/does-not-exist',
  },

  properties: [
    {
      class: 'String',
      name: 'url',
      documentation: 'Location of file containing JSON array.',
      required: true,
    },
    {
      class: 'Array',
      of: 'String',
      name: 'safeProtocols',
      required: true,
    },
    {
      class: 'Array',
      of: 'String',
      name: 'safeHostnames',
      required: true,
    },
    {
      class: 'Array',
      of: 'String',
      name: 'safePathPrefixes',
      required: true,
    },
    {
      class: 'FObjectProperty',
      of: 'foam.json.Parser',
      name: 'parser',
      transient: true,
      factory: function() {
        // NOTE: Configuration must be consistent with outputters in
        // corresponding foam.dao.RestDAO.
        return this.Parser.create({
          strict: true,
          creationContext: this.ClassWhitelistContext.create({
            whitelist: require('../../data/class_whitelist.json'),
          }, this).__subContext__,
        });
      },
    },
    {
      class: 'Function',
      name: 'createURL',
      documentation: 'Platform-independent URL factory function.',
      transient: true,
      factory: function() {
        return foam.isServer ?
            str => require('url').parse(str) :
            str => new URL(str);
      },
    },
    {
      name: 'promise',
      transient: true,
      factory: function() {
        try {
          this.validate();
        } catch (error) {
          return Promise.reject(error);
        }

        const url = this.url;
        return this.HTTPRequest.create({
          url,
          responseType: 'text',
        }).send().then(response => {
          if (response.status !== 200) throw response;
          return response.payload;
        }).then(jsonStr => {
          const array = this.parser.parseClassFromString(jsonStr, this.of);
          foam.assert(Array.isArray(array), 'HttpJsonDAO: Expected array');

          let dao = this.MDAO.create({
            of: this.of,
            propertyIndexGroups: this.propertyIndexGroups,
          });

          // Recontextualize items in DAO context. Safe to resolve immediately
          // because MDAO.put() is synchronous.
          array.forEach(item => dao.put(item.clone(dao)));

          return dao;
        });
      },
    },
  ],

  methods: [
    function validate() {
      this.SUPER();
      const url = this.url;
      if (!foam.String.isInstance(url))
        throw new Error('HttpJsonDAO: URL is not a String');
      if (!this.urlIsSafe(this.createURL(url)))
        throw new Error(`HttpJsonDAO: URL is not safe: ${url}`);
    },
    function urlIsSafe(url) {
      return this.safeProtocols.includes(url.protocol) &&
          this.safeHostnames.includes(url.hostname) &&
          this.safePathPrefixes.some(prefix => url.pathname.startsWith(prefix));
    },
  ],
});

foam.CLASS({
  package: 'org.chromium.apis.web',
  name: 'SerializableHttpJsonDAO',
  extends: 'org.chromium.apis.web.SerializableIndexedDAO',

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
      class: 'Array',
      of: 'String',
      name: 'safeProtocols',
      factory: function() { return ['https:']; },
    },
    {
      class: 'Array',
      of: 'String',
      name: 'safeHostnames',
      factory: function() { return ['localhost', 'storage.googleapis.com']; },
    },
    {
      class: 'Array',
      of: 'String',
      name: 'safePathPrefixes',
      factory: function() { return ['/web-api-confluence-data-cache/']; },
    },
    {
      name: 'delegate',
      factory: function() {
        this.validate();
        return foam.lookup('org.chromium.apis.web.HttpJsonDAO').create({
          of: this.of,
          propertyIndexGroups: this.propertyIndexGroups,
          url: this.url,
          safeProtocols: this.safeProtocols,
          safeHostnames: this.safeHostnames,
          safePathPrefixes: this.safePathPrefixes,
        });
      },
    },
  ],
});
