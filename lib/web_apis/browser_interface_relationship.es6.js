foam.RELATIONSHIP({
  sourceModel: 'org.chromium.apis.web.Browser',
  targetModel: 'org.chromium.apis.web.WebInterface',
  forwardName: 'interfaces',
  inverseName: 'browsers',
  cardinality: '*:*',
});
