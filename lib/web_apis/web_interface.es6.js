foam.CLASS({
  name: 'WebInterface',
  package: 'com.web.api',
  properties: [
    'interfaceName',
    'APIName',
    'id',
  ],
  methods: [
    {
      name: 'init',
      document: `After the WebInterface is created, hashCode function is used
        to assign this browser a unique browser id`,
      code: function() {
        this.id = foam.util.hashCode(this);
      },
    },
  ],
});
