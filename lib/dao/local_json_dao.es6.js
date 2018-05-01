// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

require('./indexed_dao.es6.js');

foam.CLASS({
  package: 'org.chromium.apis.web',
  name: 'LocalJsonDAO',
  extends: 'foam.dao.PromisedDAO',
  implements: ['org.chromium.apis.web.AbstractFacetedIndexed'],

  documentation: `DAO that fetches a JSON array from a local file. Note: the DAO
      does NOT write back to the local file on update.`,

  requires: [
    'foam.box.ClassWhitelistContext',
    'foam.json.Parser',
    'foam.net.HTTPRequest',
    'org.chromium.apis.web.MDAO',
  ],

  properties: [
    {
      class: 'String',
      name: 'path',
      documentation: 'Location of file containing JSON array.',
      required: true,
    },
    {
      class: 'FObjectProperty',
      of: 'foam.json.Parser',
      name: 'parser',
      transient: true,
      factory: function() {
        return this.Parser.create({
          strict: true,
          creationContext: this.ClassWhitelistContext.create({
            whitelist: require('../../data/class_whitelist.json'),
          }, this).__subContext__,
        });
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

        const path = this.path;
        return new Promise((resolve, reject) => {
          this.fs_.readFile(path, (error, data) => {
            const jsonStr = data.toString();
            const array = this.parser.parseClassFromString(jsonStr, this.of);

            if (!Array.isArray(array)) {
              reject(new Error('LocalJsonDAO: Expected array'));
              return;
            }

            let dao = this.MDAO.create({
              of: this.of,
              propertyIndexGroups: this.propertyIndexGroups,
            });

            // Recontextualize items in DAO context. Safe to resolve immediately
            // because MDAO.put() is synchronous.
            array.forEach(item => dao.put(item.clone(dao)));
            resolve(dao);
          });
        });
      },
    },
    {
      name: 'fs_',
      factory: function() { return require('fs'); },
    },
  ],
});

foam.CLASS({
  package: 'org.chromium.apis.web',
  name: 'SerializableLocalJsonDAO',
  extends: 'org.chromium.apis.web.SerializableIndexedDAO',

  documentation: `A proxy that can be instantiated as a LocalJsonDAO
      configuration (without loading data) in one context, and then deployed
      (by loading data) in another. This is achieved by using a lazy factory
      on a transient "delegate" property; only in contexts where the "delegate"
      property is accessed will fetch data.`,

  requires: ['org.chromium.apis.web.LocalJsonDAO'],

  properties: [
    {
      class: 'String',
      name: 'path',
      documentation: 'Configuration passed to delegate.',
      required: true,
    },
    {
      name: 'delegate',
      factory: function() {
        this.validate();
        return foam.lookup('org.chromium.apis.web.LocalJsonDAO').create({
          of: this.of,
          propertyIndexGroups: this.propertyIndexGroups,
          path: this.path,
        });
      },
    },
  ],
});
