# Data input/output directory

This directory contains data input and output for NodeJS code.

- `class_whitelist.json`: This is a list of FOAM classes that are safe to
  serialize/deserialize. **Known issue**: Classes in `foam.core` are listed
  twice: with and without package path. This is because applying the default
  package name occurs after the whitelist check.

- `http_json_dao_base_url.json`: A JSON-compatible string containing the base
  URL for loading DAO contents over HTTP. NodeJS scripts/services can be
  configured to use this base URL in `"HTTP"` mode, or to load data from
  `data/json` in `"LOCAL"` mode.

- `version_history.json`: Browser name + version prefixes to release date
  mappings.

- `json/`: JSON files loaded by NodeJS scripts/services in `"LOCAL"` mode.

- `object-graph/`: JSONified JavaScript object graphs (from `object-graph-js`
  npm module) that act as inputs to the `main/og_to_json.es6.js` NodeJS script.
