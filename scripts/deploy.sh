#!/bin/zsh

WD=$(readlink -f $(dirname "$0"))

GREEN='\033[0;32m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
NC='\033[0m'

function win() {
  printf "\n${GREEN}$1${NC}\n"
}

function warn() {
    printf "\n${YELLOW}$1${NC}\n"
}

function error() {
    printf "\n${RED}$1${NC}\n"
}

pushd "${WD}/.." > /dev/null
CONFLUENCE_DIFF_OUTPUT=$(git diff --stat)
CONFLUENCE_CLEAN_OUTPUT=$(git clean --dry-run)
CONFLUENCE_VERSION=$(git rev-parse HEAD)
popd > /dev/null

pushd "${WD}/../node_modules/foam2" > /dev/null
FOAM2_DIFF_OUTPUT=$(git diff --stat)
FOAM2_CLEAN_OUTPUT=$(git clean --dry-run)
FOAM2_BRANCH=$(git rev-parse --abbrev-ref HEAD)
FOAM2_VERSION=$(git rev-parse HEAD)
popd > /dev/null

if [[ "${CONFLUENCE_VERSION}" = "" ]]; then
  error "Main source is not under version control. Is this not a git clone?"
  exit 1
fi
if [[ "${FOAM2_VERSION}" = "" || "${CONFLUENCE_VERSION}" = "${FOAM2_VERSION}" ]]; then
  error "FOAM2 is not under version control. Did you forget to symlink /path/to/foam2 -> node_modules/foam2?"
  exit 1
fi
if [[ "${CONFLUENCE_DIFF_OUTPUT}" != "" || "${CONFLUENCE_CLEAN_OUTPUT}" != "" ]]; then
  error "Your working tree is not clean"
  exit 1
fi
if [[ "${FOAM2_DIFF_OUTPUT}" != "" || "${FOAM2_CLEAN_OUTPUT}" != "" ]]; then
  error "FOAM2 working tree is not clean"
  exit 1
fi
# TODO(markdittmer): Keep package.json branch name and this branch name in sync.
if [[ "${FOAM2_BRANCH}" != "confluence" ]]; then
  error "FOAM2 checked out to wrong branch: ${FOAM2_BRANCH}"
  exit 1
fi
win "Preparing build for Confluence@${CONFLUENCE_VERSION}, FOAM2@${FOAM2_VERSION}"

if ! webpack --config "${WD}/../config/webpack.prod.js"; then
  error "webpack failed"
  exit 1
fi
win "Deploying"

if gcloud config set project web-confluence && \
    gcloud app deploy --no-promote --version="confluence-${CONFLUENCE_VERSION:0:7}--foam2-${FOAM2_VERSION:0:7}"; then
  win "App deployed! Exiting."
else
  error "App deployment failed"
  exit 1
fi
