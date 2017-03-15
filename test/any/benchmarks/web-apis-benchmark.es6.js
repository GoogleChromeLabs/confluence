
require('foam2');
require('../../../lib/web_catalog/api_extractor.es6');
require('../../../lib/web_apis_num_id/web_apis.es6');
require('../../../lib/web_apis_one_table/web_apis.es6');
require('../../../lib/web_apis_string_array_id/web_apis.es6');
const objectGraph = require('object-graph-js').ObjectGraph;
const fs = require('fs');

const OG_DATA_PATH = `${__dirname}/../../../data`;

let extractor = com.web.catalog.apiExtractor.create({});
let webAPIsNumId = com.web.api.WebAPIs_numId.create();
let webAPIsStrId = com.web.api.WebAPIs_StrId.create();
let webAPIsOneTable = com.web.api.WebAPIs_oneTable.create();

// Read all og data files from OG_DATA_PATH.
let ogFiles = fs.readdirSync(OG_DATA_PATH);
let interfaceMaps = {};

console.warn('Start reading files');
console.time('read-og-files');
for (let i = 0; i < ogFiles.length; i += 1) {
  console.info(`readFile ${ogFiles[i]} --- ` +
    Math.floor(i * 100 / ogFiles.length) + '%');
  let filePath = `${OG_DATA_PATH}/${ogFiles[i]}`;
  let stat = fs.statSync(filePath);
  if (stat.isFile()) {
    let fileName = ogFiles[i].slice(0, -5);
    //
    // if (fileName.split('_')[1] !== 'Edge') continue;
    //
    interfaceMaps[fileName] = extractor.extractWebCatalog(objectGraph
      .fromJSON(JSON.parse(fs.readFileSync(filePath))));
  }
}
console.timeEnd('read-og-files');
console.warn('Read files Done');

// 3 tables with numeric id. (need to use join to get interface name)

console.time('numeric-id-import-API');
for (browser in interfaceMaps) {
  if (interfaceMaps.hasOwnProperty(browser)) {
    let browserSpec = browser.split('_');
    webAPIsNumId.importAPI(browserSpec[1], browserSpec[2], browserSpec[3],
      browserSpec[4], interfaceMaps[browser]);
  }
}
console.timeEnd('numeric-id-import-API');

console.time('numeric-id-total-time');
webAPIsNumId.toCSV([
  'Chrome_55.0.2883.75_OSX_10.11.6',
  'Chrome_55.0.2883.75_Windows_10.0',
  'Chrome_56.0.2924.76_OSX_10.11.6',
  'Chrome_56.0.2924.87_Windows_10.0',
  'Edge_13.10586_Windows_10.0',
  'Edge_14.14393_Windows_10.0',
  'Firefox_34.0_OSX_10.11',
  'Firefox_35.0_Windows_10.0',
  'Firefox_36.0_OSX_10.11',
  'Firefox_36.0_Windows_10.0',
]).then((csv) => {
  // console.log('###numeric id###', csv);
  console.timeEnd('numeric-id-total-time');

  // 3 tables with string array id. (Get interface name from interface id).
  console.time('string-array-id-import-API');
  for (browser in interfaceMaps) {
    if (interfaceMaps.hasOwnProperty(browser)) {
      let browserSpec = browser.split('_');
      webAPIsStrId.importAPI(browserSpec[1], browserSpec[2], browserSpec[3],
        browserSpec[4], interfaceMaps[browser]);
    }
  }
  console.timeEnd('string-array-id-import-API');

  console.time('string-array-id-total-time');
  webAPIsStrId.toCSV([
    'Chrome_55.0.2883.75_OSX_10.11.6',
    'Chrome_55.0.2883.75_Windows_10.0',
    'Chrome_56.0.2924.76_OSX_10.11.6',
    'Chrome_56.0.2924.87_Windows_10.0',
    'Edge_13.10586_Windows_10.0',
    'Edge_14.14393_Windows_10.0',
    'Firefox_34.0_OSX_10.11',
    'Firefox_35.0_Windows_10.0',
    'Firefox_36.0_OSX_10.11',
    'Firefox_36.0_Windows_10.0',
  ]).then((csv) => {
    // console.log('###stirng array id###', csv);
    console.timeEnd('string-array-id-total-time');

    // One table implementation.
    console.time('one-table-import-API');
    for (browser in interfaceMaps) {
      if (interfaceMaps.hasOwnProperty(browser)) {
        let browserSpec = browser.split('_');
        webAPIsOneTable.importAPI(browserSpec[1], browserSpec[2],
          browserSpec[3], browserSpec[4], interfaceMaps[browser]);
      }
    }
    console.timeEnd('one-table-import-API');

    console.time('one-table-total-time');
    webAPIsOneTable.toCSV([
      'Chrome_55.0.2883.75_OSX_10.11.6',
      'Chrome_55.0.2883.75_Windows_10.0',
      'Chrome_56.0.2924.76_OSX_10.11.6',
      'Chrome_56.0.2924.87_Windows_10.0',
      'Edge_13.10586_Windows_10.0',
      'Edge_14.14393_Windows_10.0',
      'Firefox_34.0_OSX_10.11',
      'Firefox_35.0_Windows_10.0',
      'Firefox_36.0_OSX_10.11',
      'Firefox_36.0_Windows_10.0',
    ]).then((csv) => {
      // console.log('###one table###',csv);
      console.timeEnd('one-table-total-time');
    });
  });
});
