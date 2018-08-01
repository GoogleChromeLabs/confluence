#!/bin/zsh

#
# Perform all steps to transform local data/object-graph/*.json files into all
# Confluence data models used in production.
#

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

og_to_json_PID=""
relational_to_compat_PID=""
json_to_metrics_PID=""

function stop() {
  warn "STOPPING OG-TO-JSON (PID=${og_to_json_PID})"
  if [ "${og_to_json_PID}" != "" ]; then kill "${og_to_json_PID}"; fi
  win "OG-TO-JSON STOPPED"
  warn "STOPPING RELATIONAL-TO-COMPAT (PID=${relational_to_compat_PID})"
  if [ "${relational_to_compat_PID}" != "" ]; then kill "${relational_to_compat_PID}"; fi
  win "RELATIONAL-TO-COMPAT STOPPED"
  warn "STOPPING RELATIONAL-TO-GRID (PID=${relational_to_grid_PID})"
  if [ "${relational_to_grid_PID}" != "" ]; then kill "${relational_to_compat_PID}"; fi
  win "RELATIONAL-TO-GRID STOPPED"
  warn "STOPPING JSON-TO-METRICS (PID=${json_to_metrics_PID})"
  if [ "${json_to_metrics_PID}" != "" ]; then kill "${json_to_metrics_PID}"; fi
  win "JSON-TO-METRICS STOPPED"
  exit 0
}

trap stop INT

warn "STARTING  OG-TO-JSON"
node "${WD}/../main/og_to_json.es6.js" &
og_to_json_PID=$!
win "OG-TO-JSON STARTED (PID=${og_to_json_PID})"

wait "${og_to_json_PID}"
og_to_json_status="${?}"
og_to_json_PID=""

if [ "${og_to_json_status}" != "0" ]; then
  error "OG-TO-JSON FAILED"
  exit "${og_to_json_status}"
fi


warn "STARTING RELATIONAL-TO-COMPAT"
node "${WD}/../main/relational_to_compat.es6.js" "file://${WD}/../data/json" &
relational_to_compat_PID=$!
win "RELATIONAL-TO-COMPAT STARTED (PID=${relational_to_compat_PID})"

warn "STARTING RELATIONAL-TO-GRID"
node "${WD}/../main/relational_to_grid.es6.js" "file://${WD}/../data/json" &
relational_to_grid_PID=$!
win "RELATIONAL-TO-GRID STARTED (PID=${relational_to_grid_PID})"

wait "${relational_to_compat_PID}"
relational_to_compat_PID=""
relational_to_compat_status="${?}"
if [ "${relational_to_compat_status}" != "0" ]; then
  error "RELATIONAL-TO-COMPAT FAILED"
  exit "${relational_to_compat_status}"
fi


warn "STARTING JSON-TO-METRICS"
node "${WD}/../main/json_to_metrics.es6.js"
json_to_metrics_PID=$!
win "JSON-TO-METRICS STARTED (PID=${json_to_metrics_PID})"

wait "${relational_to_grid_PID}"
relational_to_grid_PID=""
relational_to_grid_status="${?}"
if [ "${relational_to_grid_status}" != "0" ]; then
  error "RELATIONAL-TO-GRID FAILED"
  exit "${relational_to_grid_status}"
fi

wait "${json_to_metrics_PID}"
json_to_metrics_PID=""
json_to_metrics_status="${?}"
if [ "${json_to_metrics_status}" != "0" ]; then
  error "JSON-TO-METRICS FAILED"
  exit "${json_to_metrics_status}"
fi
