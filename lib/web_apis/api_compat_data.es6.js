// Copyright 2018 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.
'use strict';

require('../property.es6.js');
require('./release.es6.js');

foam.CLASS({
  name: 'CompatProperty',
  package: 'org.chromium.apis.web',
  extends: 'foam.core.Boolean',

  documentation: `A Boolean property that contains a browser release as
      metadata. This pattern is used for per-API compat data.`,

  requires: ['org.chromium.apis.web.Release'],

  css: `
    .org-chromium-apis-web-CompatProperty {
      display: flex;
      justify-content: center;
      align-items: center;
    }

    .org-chromium-apis-web-CompatProperty.false {
      background-color: #FF8A80;
    }

    .org-chromium-apis-web-CompatProperty.true {
      background-color: #B9F6CA;
    }
  `,

  properties: [
    {
      class: 'FObjectProperty',
      of: 'org.chromium.apis.web.Release',
      name: 'release',
      documentation: `The browser release associated with this property.`,
    },
    {
      name: 'rawTableCellFormatter',
      documentation: `A raw markup outputter for table cells outputting this
          property's value.`,
      value: function(value, obj, axiom) {
        // Apply CSS class associated with this FOAM class.
        const cls = 'org-chromium-apis-web-CompatProperty';
        // Apply CSS class associated with value.
        const valueCls = value === true ? 'true' : value === false ?
              'false' : '';
        // Select Material icon text for checkmark, X, or hourglass (loading),
        // according to value.
        //
        // See https://material.io/icons/
        const iconValue = value === true ? 'check' : value === false ?
              'clear' : 'hourglass_empty';

        return `
          <div class="${cls} ${valueCls}">
            <span class="material-icons">${iconValue}</span>
          </div>
        `;
      },
    },
  ],

  methods: [
    {
      name: 'toString',
      documentation: `Provide something more useful than
          [FOAM class package path] as string representation`,
      code: function() { return this.name; },
    },
  ],
});

foam.CLASS({
  name: 'AbstractApiCompatData',
  package: 'org.chromium.apis.web',

  documentation: `Abstract class underpinning generated API compat data class

      This class is the basis for a generated class with a CompatProperty per
      release, detailing all the browser compatability data for a single
      API. This representation of the compat data is well aligned with FOAM
      features such as U2 table views, query parsers, MDAO indices, etc..`,

  properties: [
    {
      class: 'String',
      name: 'id',
      documentation: `ID of each datum is composed from interface + interface
          member name. Markup outputter enables CSS overlay pattern to
          conditionally overlay ID text over other table cells.`,
      label: 'API',
      expression: function(interfaceName, apiName) {
        return `${interfaceName}#${apiName}`;
      },
      hidden: true,
      rawTableCellFormatter: function(value, obj, axiom) {
        const textValue = foam.String.isInstance(value) ? value : '&nbsp;';
        return `
          <div class="id">
            ${textValue}
            <div class="overlay"><span>${textValue}</span></div>
          </div>
        `;
      },
    },
    {
      class: 'String',
      name: 'interfaceName',
      required: true,
    },
    {
      class: 'String',
      name: 'apiName',
      required: true,
    },
  ],
});
