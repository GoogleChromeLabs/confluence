// Copyright 2017 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

foam.CLASS({
  package: 'org.chromium.apis.web',
  name: 'AbstractFacetedIndexed',

  documentation: `Abstract class for something that is faceted (stores a Class
      in "of") and indexed according to "indices" property.`,

  properties: [
    {
      class: 'Class',
      name: 'of',
    },
    {
      class: 'Array',
      name: 'indices',
      documentation: 'Indices for indexing instances of "of" class.',
      adapt: function(_, nu) {
        return nu.map(strArr => Array.isArray(strArr) ? strArr : [strArr]);
      },
      preSet: function(_, nu) {
        nu.forEach(strArr => strArr.forEach(
            propName => foam.assert(foam.core.Property.isInstance(
                this.of.getAxiomByName(propName)),
                `Expected "${this.of && this.of.id}" to contain property
                    "${propName}"`)));
        return nu;
      },
    },
  ],
});

foam.CLASS({
  package: 'org.chromium.apis.web',
  name: 'MDAO',
  extends: 'foam.dao.MDAO',
  implements: ['org.chromium.apis.web.AbstractFacetedIndexed'],

  documentation: `Extension of MDAO that uses "indices" to specify property
      indices.`,

  properties: [
    {
      name: 'promise',
      transient: true,
      factory: function() {
        throw new Error(`Concrete JsonDAO "${this.cls_.id}" without required
            promise factory`);
      },
    },
  ],

  methods: [
    function init() {
      this.SUPER();
      this.indices
          .forEach(index => this.addPropertyIndex.apply(this, index));
    },
  ],
});

foam.CLASS({
  package: 'org.chromium.apis.web',
  name: 'SerializableIndexedDAO',
  extends: 'foam.dao.ProxyDAO',
  implements: ['org.chromium.apis.web.AbstractFacetedIndexed'],

  documentation: `Base class for serializable, indexable DAO descriptions.`,

  properties: [
    {
      class: 'foam.dao.DAOProperty',
      name: 'delegate',
      documentation: 'Transient DAO that will fetch data when instantiated.',
      transient: true,
    },
  ],
});
