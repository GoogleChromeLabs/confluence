#!/usr/bin/env sh

set -v


BASE_DIR=$(readlink -f $(dirname "$0"))

export PATH="$BASE_DIR/google-cloud-sdk/bin:$PATH"

GCLOUD_SDK_URL="https://dl.google.com/dl/cloudsdk/channels/rapid/downloads/google-cloud-sdk-145.0.0-linux-x86_64.tar.gz"
GCLOUD_SDK_TGZ="$BASE_DIR/google-cloud-sdk.tar.gz"
GCLOUD_PATH="$(which gcloud)"

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

if [ "$GCLOUD_PATH" == "" ]; then
  curl -o "$GCLOUD_SDK_TGZ" "$GCLOUD_SDK_URL"
  pushd "$BASE_DIR"
  tar xzvf "$GCLOUD_SDK_TGZ"
  "./google-cloud-sdk/install.sh" -q --override-components cloud-datastore-emulator
  popd
fi

if [ "$(which gcloud)" == "" ]; then
  error "Failed to install gcloud"
  exit 1
fi
