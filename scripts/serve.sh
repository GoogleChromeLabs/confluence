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

Webpack_PID=""
WebServer_PID=""

function stop() {
  warn "STOPPING WEBPACK (PID=${Webpack_PID})"
  if [ "${Webpack_PID}" != "" ]; then kill "${Webpack_PID}"; fi
  win "WEBPACK STOPPED"
  warn "STOPPING WEB SERVER (PID=${WebServer_PID})"
  if [ "${WebServer_PID}" != "" ]; then kill "${Webpack_PID}"; fi
  win "WEB SERVER STOPPED"
  exit 0
}

trap stop INT

mkdir -p "${WD}/../.local"

warn "STARTING WEBPACK"
webpack --watch --progress --config "${WD}/../config/webpack.dev.js" &
Webpack_PID=$!
win "WEBPACK STARTED (PID=${Webpack_PID})"

warn "STARTING WEB SERVER"
node --max_old_space_size=4000 "${WD}/../main/serve.js" "LOCAL" "DEV" &
WebServer_PID=$!
win "WEB SERVER STARTED (PID=${WebServer_PID})"

wait
