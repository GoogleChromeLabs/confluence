// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

foam.CLASS({
  package: 'org.chromium.apis.web',
  name: 'HttpJDAO',
  extends: 'foam.dao.PromisedDAO',

  documentation: `DAO that fetches a journal via HTTP, then produces a JDAO that
      loads the journal.`,

  requires: [
    'foam.dao.JDAO',
    'foam.net.HTTPRequest',
  ],

  constants: {
    SAFE_URL_PROTOCOL: 'https:',
    SAFE_URL_HOSTNAME: 'storage.googleapis.com',
    SAFE_URL_PATHNAME_PREFIX: '/web-api-confluence-data-cache/',
    ERROR_URL: 'https://storage.googleapis.com/web-api-confluence-data-cache/does-not-exist',
  },

  classes: [
    {
      name: 'StringJournal',

      properties: [
        {
          class: 'Class',
          name: 'of',
          value: 'foam.core.FObject'
        },
        {
          class: 'String',
          name: 'str',
          documentation: 'String representation of journal.',
        },
      ],

      methods: [
        function replay(dao) {
          const self = this;
          return new Promise((resolve, reject) => {
            const context = {
              put: function(o) { return dao.put(o); },
              remove: function(o) { return dao.remove(o); },
              foam: {
                json: {
                  parse: (obj) => {
                    return foam.json.parse(obj, self.of, dao.__context__);
                  }
                }
              }
            };

            with (context) {
              eval(self.str);
            }

            resolve(dao);
          });
        },
      ],
    },
  ],

  properties: [
    {
      name: 'url',
      documentation: 'Location of journal file.',
      preSet: function(old, nu) {
        // Thwart attempts to set URL to something unsafe. I.e., only trust code
        // from URLs matching "SAFE_*" constants.
        if (!foam.String.isInstance(nu)) return this.ERROR_URL;
        const url = this.createURL(nu);
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
      class: 'foam.dao.DAOProperty',
      name: 'delegate',
      documentation: 'Delegate for JDAO.',
      required: true,
    },
    {
      class: 'Function',
      name: 'createURL',
      documentation: 'Platform-independent URL factory function.',
      factory: function() {
        const URLCtor = foam.isServer ? require('url').URL : URL;
        return function(str) { return new URLCtor(str); };
      },
    },
    {
      name: 'promise',
      factory: function() {
        this.validate();
        const url = this.url;
        return this.HTTPRequest.create({url}).send().then(response => {
          if (response.status !== 200) throw response;
          return response.payload;
        }).then(data => {
          return this.JDAO.create({
            of: this.of,
            delegate: this.delegate,
            journal: this.StringJournal.create({
              of: this.of,
              str: data.toString(),
            }),
          }).promise;
        });
      },
    },
  ],
});
