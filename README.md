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
- [The Metrics](#the-metrics)
  - [API Velocity](#api-velocity)
    - [Definition](#definition)
    - [Rationale](#rationale)
  - [Failure to Ship](#failure-to-ship)
    - [Definition](#definition-1)
    - [Rationale](#rationale-1)
  - [Aggressive Removal](#aggressive-removal)
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

## The Metrics

*API confluence metrics* are a count of “APIs” that meet specific criteria with
respect to browser releases that include these “APIs”.

**Definition**: *API*: For the purposes of these metrics, an “API” is an
interface name + attribute or operation pair.

**Definition**: *The Browser*: Each *API Confluence Metric* is computed with
respect to some particular browser; this is what’s meant by The Browser. E.g.,
the “Aggressive Removal metric for Safari on 2016-09-01” describes APIs that
Safari once provided (but no longer does) that where the latest release of all
*other* browsers a year later contains the APIs; in this case Safari is The
Browser.

**Definition**: *Grace Period*: Most metrics are deliberately calculated with
respect to releases of browsers *other than* The Browser sometime in the
past. This avoids penalizing The Browser for making a change (i.e., shipping
or removing an API) when other browsers respond in kind. Currently, the Grace
Period used for all metrics that have one is one year. The "a year later" in
the above example refers to the Aggressive Removal Grace Period.

*API Confluence metrics* are API counts assessed for a particular browser at a
particular point in time. Most metrics are computed on every date that *any*
browser has a major release. Some metrics are only computed on dates when The
Browser has a major release.

### API Velocity

#### Definition

The API Velocity metric contains three values; the total number of APIs provided
as of the latest browser release, the number of APIs removed (since the previous
release) and the number of APIs added (since the previous release). This metric
is computed on dates when The Browser has a major release.

#### Rationale

When browsers move too slowly, it holds back the platform. When browsers move
too quickly, they risk “leaving other browsers behind”. Steady growth is good;
wild variation is bad.

### Failure to Ship

#### Definition

The Failure to Ship metric indicates the number of APIs that The Browser
provides for the duration of the Grace Period, but all other browsers do
provide throughout the Grace Period.

#### Rationale

Failing to ship an API that other major vendors provide requires web
developers to use special code paths to remain interoperable. Smaller values
are good; larger values are bad.

### Aggressive Removal

#### Definition

The Aggressive Removal metric indicates the number of APIs removed from a The
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

1. Before cloning this repository you
   should [install git lfs](https://git-lfs.github.com/). We use Git LFS to
   check in database snapshots. This improves the local debugging experience (no
   need to hit remote APIs to access data locally) and improves production
   deployment (snapshots bootstrap local caches).

2. Clone this repository.

3. Use `npm run serve` to launch a local instance of the service. This will look
   for `.local/credentials.json`, and when it doesn’t find it, use the local
   snapshot of data, rather than attempting to connect to a data backend. The
   last data source to come up in the release-to-API associations (usually takes
   about a minute).

4. Hack away! `npm run serve` uses `webpack --watch` to observe local
   changes. Making changes to server code will require a service restart, but
   client-side changes will be reflected soon after they are saved.

### Collecting data

**NOTE**: The current data collection process requires
a [BrowserStack](https://www.browserstack.com/)
account, [Cloud Datastore](https://cloud.google.com/datastore/) credentials, two
separate `git clone`s, and a whole lot of RAM. We hope to streamline and
simplify this process soon. If you have all the prerequisites, read on…

1. Clone [mdittmer/web-apis](https://github.com/mdittmer/web-apis) and follow
   the
   [data collection instructions](https://github.com/mdittmer/web-apis#setup-browserstack) for
   *historical data collection using BrowserStack*.

2. Copy `/path/to/mdittmer/web-apis/data/og/*.json` to
   `/path/to/GoogleChrome/confluence/data/og`.

3. In `/path/to/GoogleChrome/confluence`, install your Cloud Datastore
   credentials at `.local/credentials.json`.

4. Run `node --max_old_space_size=16384 ./main/datastore_update.es6.js`. This
   will take a long time, but it will push new data to your Cloud Datastore
   database (default partition).

5. Optional: run `node --max_old_space_size=16384 ./main/journal_update.es6.js`
   to update your local data snapshot. This is not strictly necessary, but Cloud
   Datastore-backed service instances will take longer to initialize their cache
   if they have a stale local snapshot.

**Caveat**: In order to serve the data you collect, you must ensure that a `{
<browser name}: { <browser version prefix>: <release date> } }` for every
version you have imported appears in `data/version_history.json`.
