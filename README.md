# [Web API Confluence Dashboard](https://web-confluence.appspot.com) [![Build Status](https://travis-ci.org/GoogleChrome/confluence.svg?branch=master)](https://travis-ci.org/GoogleChrome/confluence)[![Codecov](https://img.shields.io/codecov/c/github/GoogleChrome/confluence.svg)]()

A web service and UI for describing *API confluence metrics*. These metrics are
intended to capture the how browser vendors
are
[stretching the elastic band](https://docs.google.com/presentation/d/1pfu-wAxbkVN41Zgg9P3ln9tJB9AwKh9T3btyWvd17Rk/edit#slide=id.g1c2be92856_0_10) of
the web platform.

**Stretching is good**: Browsers should be implementing new APIs to add value to
the platform.

**Breaking is bad**: Implementing *too many* new APIs before other browsers
catch up, or failing to remove APIs other browsers don't intend to ship causes
fragmentation.

The purpose of *API Confluence Metrics* is to capture the ways in which
different browsers risk breaking the elastic band.

Data collected via [BrowserStack](https://www.browserstack.com).

<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**  *generated with [DocToc](https://github.com/thlorenz/doctoc)*

- [The Catalog](#the-catalog)
  - [Querying the catalog](#querying-the-catalog)
    - [Examples](#examples)
- [The Metrics](#the-metrics)
  - [API Count](#api-count)
    - [Definition](#definition)
    - [Rationale](#rationale)
  - [Lone Omission](#lone-omission)
    - [Definition](#definition-1)
    - [Rationale](#rationale-1)
  - [Lone Removal](#lone-removal)
    - [Definition](#definition-2)
    - [Rationale](#rationale-2)
  - [Browser-Specific](#browser-specific)
    - [Definition](#definition-3)
    - [Rationale](#rationale-3)
- [Contributing](#contributing)
  - [Filing issues and contributing code](#filing-issues-and-contributing-code)
  - [Running locally](#running-locally)
  - [Collecting data](#collecting-data)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

## The Catalog

The dashboard contains an API catalog that lists attributes/properties and
operations/methods exposed on constructor functions and their prototypes. The
catalog constitutes the raw data from which aggregate *API confluence metrics*
are computed. See [CatalogDataCollection.md](/CatalogDataCollection.md) for
details on how the catalog is created.

### Querying the catalog

The catalog supports structured queries. Some query atoms apply to *all*
cataloged browser releases, while others apply to the releases currently
*in view* (i.e., the releases currently shown as columns in the table of APIs).

Query atoms may be joined by whitespace, conjunction (`and` or `&`), or
disjunction (`or` or `|`), with parentheses to disambiguate as needed. Atoms are
one of the following:

- **(Not-)in-releases clause**: A phrase of the form `in:release` or
  `notin:release` where `release` is identified by case-insensitive
  `[release-name-prefix][release-version-prefix][os-name-prefix][os-version-prefix]`.
  Any of these, except `[release-name-prefix]` may be empty. For example,
  `in:fir59` describes APIs shipped in *all* releases of Firefox 59 (that are
  included in the catalog). These atoms apply to *all* releases.
- **Count-of-releases clause**: A phrase of the form `count:n` where `n` is a
  non-negative integer describes APIs that are shipped in exactly `n` releases
  currently *in view*.
- **Keyword**: An atom matching the regular expression `[a-zA-Z0-9_#-]+`
  describes APIs that contain the atom by case-insensitive substring match.

#### Examples

`window# count:1`: APIs on intefaces with the case-insensitive `window` suffix
that are shipped in exactly one of the releases in view.

`count:1 or count:2 or count:3 or count:4`: On a view with showing four or fewer
releases, APIs that are shipped by at least one release in view.

`in:chrome65 and notin:chrome66`: APIs removed in Chrome 66.

## The Metrics

*API confluence metrics* are a count of “APIs” that meet specific criteria with
respect to browser releases that include these “APIs”.

**Definition**: *API*: For the purposes of these metrics, an “API” is an
interface name + attribute or operation pair.

**Definition**: *The Browser*: Each *API Confluence Metric* is computed with
respect to some particular browser; this is what’s meant by The Browser. E.g.,
the “Lone Removal metric for Safari on 2016-09-01” describes APIs that
Safari once provided (but no longer does) that where the latest release of all
*other* browsers a year later contains the APIs; in this case Safari is The
Browser.

**Definition**: *Grace Period*: Most metrics are deliberately calculated with
respect to releases of browsers *other than* The Browser sometime in the
past. This avoids penalizing The Browser for making a change (i.e., shipping
or removing an API) when other browsers respond in kind. Currently, the Grace
Period used for all metrics that have one is one year. The "a year later" in
the above example refers to the Lone Removal Grace Period.

*API Confluence metrics* are API counts assessed for a particular browser at a
particular point in time. Most metrics are computed on every date that *any*
browser has a major release. Some metrics are only computed on dates when The
Browser has a major release.

### API Count

#### Definition

The API Count metric contains three values; the total number of APIs provided
as of the latest browser release, the number of APIs removed (since the previous
release) and the number of APIs added (since the previous release). This metric
is computed on dates when The Browser has a major release.

#### Rationale

When browsers move too slowly, it holds back the platform. When browsers move
too quickly, they risk “leaving other browsers behind”. Steady growth is good;
wild variation is bad.

### Lone Omission

#### Definition

The Lone Omission metric indicates the number of APIs that The Browser does
not provide provide for the duration of the Grace Period, but all other
browsers do provide throughout the Grace Period.

#### Rationale

Failing to ship an API that other major vendors provide requires web
developers to use special code paths to remain interoperable. Smaller values
are good; larger values are bad.

### Lone Removal

#### Definition

The Lone Removal metric indicates the number of APIs removed from a The
Browser prior to the Grace Period, that have not been added back in the
latest relase following the Grace Period, and that are provided in all other
browsers in the latest relase following the Grace Period.

#### Rationale

Removing an API from only one browser risks breaking existing sites that
(reasonably) assume that all browsers support the API. Smaller values are
good; larger values are bad.

### Browser-Specific

#### Definition

The Browser-Specific metric indicates the number of APIs that The Browser
provides for the duration of the Grace Period, but all other browsers do not
provide throughout the Grace Period.

#### Rationale

Adding APIs that are provided by only one browser makes that browser more and
more like its own platform (rather than an implementation of a common web
platform). Smaller values are good; larger values are bad.

## Contributing

Want to contribute to Web API Confluence? Great!

### Filing issues and contributing code

Please use GitHub’s issue tracker and pull request features.

### Running locally

1. Clone this repository.

2. Install: `npm install`

3. Launch the local server:

```bash
mkdir -p data/json
```

Then, either:

   1. Copy the latest data:

```bash
cd data/json
curl https://storage.googleapis.com/web-api-confluence-data-cache/latest/json/org.chromium.apis.web.ApiCountData.json > org.chromium.apis.web.ApiCountData.json
curl https://storage.googleapis.com/web-api-confluence-data-cache/latest/json/org.chromium.apis.web.ReleaseWebInterfaceJunction.json > org.chromium.apis.web.ReleaseWebInterfaceJunction.json
curl https://storage.googleapis.com/web-api-confluence-data-cache/latest/json/org.chromium.apis.web.BrowserMetricData.json > org.chromium.apis.web.BrowserMetricData.json
curl https://storage.googleapis.com/web-api-confluence-data-cache/latest/json/org.chromium.apis.web.WebInterface.json > org.chromium.apis.web.WebInterface.json
curl https://storage.googleapis.com/web-api-confluence-data-cache/latest/json/org.chromium.apis.web.Release.json > org.chromium.apis.web.Release.json
cd ../..
```

or

   2. [Collect the data yourself](#collecting-data).

Finally, use `npm run serve` to launch a local instance of the service. This
will load local data, which can take up to a minute to be ready to serve.

4. Hack away! `npm run serve` uses `webpack --watch` to observe local
   changes. Making changes to server code will require a service restart, but
   client-side changes will be reflected soon after they are saved.

### Collecting data

**NOTE**: The current data collection process requires a
[BrowserStack](https://www.browserstack.com/) account, two separate `git
clone`s, and a whole lot of RAM. We hope to streamline and simplify this process
soon. If you have all the prerequisites, read on…

1. Clone [mdittmer/web-apis](https://github.com/mdittmer/web-apis) and follow
   the
   [data collection instructions](https://github.com/mdittmer/web-apis#setup-browserstack) for
   *historical data collection using BrowserStack*.

2. Create `/path/to/confluence/data/object-graph` and copy
   `/path/to/web-apis/data/og/*.json` into it.

3. Create `/path/to/confluence/data/json` and run
   `./scripts/og_to_confluence.sh` to derive confluence data from the object
   graphs.

4. To run the service locally based on your generated data invoke `node
   main/serve.js "LOCAL" "DEV"`. If you want live reloading of client code,
   change the parameters passed to `main/serve.js` in `scripts/serve.sh` and
   start `webpack` alongside the service with `npm run serve.

**Caveat**: In order to serve the data you collect, you must ensure that a `{
<browser name}: { <browser version prefix>: <release date> } }` for every
version you have imported appears in `data/version_history.json`.
