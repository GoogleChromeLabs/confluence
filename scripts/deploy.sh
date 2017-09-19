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


pushd "${WD}/.."
CONFLUENCE_VERSION=$(git rev-parse HEAD)
popd

pushd "${WD}/../node_modules/foam2"
FOAM2_VERSION=$(git rev-parse HEAD)
popd

if [ "${CONFLUENCE_VERSION}" = "${FOAM2_VERSION}" ]; then
  error "FOAM2 not not under version control"
  exit 1
fi
win "Preparing build for Confluence@${CONFLUENCE_VERSION}, FOAM2@${FOAM2_VERSION}"

webpack --config "${WD}/../config/webpack.prod.js"
if [ "$?" != "0" ]; then
  error "webpack failed"
  exit 1
fi
win "Deploying"

gcloud config set project web-confluence
gcloud app deploy --version="confluence-${CONFLUENCE_VERSION:0:7}--foam2-${FOAM2_VERSION:0:7}"
win "App deployed!"
