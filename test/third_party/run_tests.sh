#!/usr/bin/env sh

set -v

BASE_DIR=$(readlink -f $(dirname "$0"))
export PATH="$BASE_DIR/google-cloud-sdk/bin:$PATH"

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

if [ "$(which gcloud)" == "" ]; then
  error "gcloud is not installed"
  exit 1
fi

CDS_EMULATOR_PID=""
JASMINE_PID=""
JASMINE_CODE=""

function stop() {
  if [ "$JASMINE_PID" != "" ]; then
    warn "STOPPING JASMINE (PID=$JASMINE_PID)"
    kill $JASMINE_PID
  fi
  win "JASMINE STOPPED"

  if [ "$CDS_EMULATOR_PID" != "" ]; then
    warn "STOPPING CLOUD DATASTORE EMULATOR (PID=$CDS_EMULATOR_PID)"
    kill $CDS_EMULATOR_PID
  fi
  win "CLOUD DATASTORE EMULATOR STOPPED"

  if [ "$JASMINE_CODE" != "" ]; then
    warn "EXIT CODE $JASMINE_CODE"
    exit $JASMINE_CODE
  else
    error "SIGINT BEFORE JASMINE EXIT"
    exit 1
  fi
}

trap stop INT

GCLOUD_BIN_DIR=$(readlink -f $(dirname $(which gcloud)))
CDS_EMULATOR_JAR="$GCLOUD_BIN_DIR/../platform/cloud-datastore-emulator/CloudDatastore.jar"
CDS_EMULATOR="$GCLOUD_BIN_DIR/../platform/cloud-datastore-emulator/cloud_datastore_emulator"

if [ ! -f "$CDS_EMULATOR" ]; then
  error "Failed to install Cloud Datastore Emulator"
  exit 1
fi

export CDS_PROJECT_ID="test-project-$RANDOM"

export CDS_EMULATOR_PROTOCOL="http"
export CDS_EMULATOR_HOST="localhost"
export CDS_EMULATOR_PORT=$((($RANDOM % 1000) + 8000))

${JAVA:="java"} -cp "$CDS_EMULATOR_JAR" \
                com.google.cloud.datastore.emulator.CloudDatastore \
                "$CDS_EMULATOR" start --host=$CDS_EMULATOR_HOST \
                --port=$CDS_EMULATOR_PORT --testing &
export CDS_EMULATOR_PID=$!
export JASMINE_CONFIG_PATH="$BASE_DIR/../../config/jasmine_gcloud.json"

# Used by datastore client library:
export DATASTORE_EMULATOR_HOST="${CDS_EMULATOR_HOST}:${CDS_EMULATOR_PORT}"
export DATASTORE_PROJECT_ID="${CDS_PROJECT_ID}"

# Run tests
sleep 5
node "$BASE_DIR/../../node_modules/.bin/jasmine" &
JASMINE_PID=$!
wait $JASMINE_PID

# OR

# Debug tests
# node --inspect --debug-brk "$BASE_DIR/../../node_modules/.bin/jasmine"

JASMINE_CODE=$?

stop
