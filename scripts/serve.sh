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

WP_PID=""
WS_PID=""

function stop() {
  warn "STOPPING WEBPACK (PID=${WP_PID})"
  if [ "${WP_PID}" != "" ]; then kill ${WP_PID}; fi
  win "WEBPACK STOPPED"
  warn "STOPPING WEB SERVER (PID=${WS_PID})"
  if [ "${WS_PID}" != "" ]; then kill ${WP_PID}; fi
  win "WEB SERVER STOPPED"
  exit 0
}

trap stop INT

warn "STARTING WEBPACK"
webpack --watch --progress --config $WD/../config/webpack.config.js &
WP_PID=$!
win "WEBPACK STARTED (PID=${WP_PID})"

warn "STARTING WEB SERVER"
node $WD/../main/serve.js &
WS_PID=$!
win "WEB SERVER STARTED (PID=${WS_PID})"

while [ true ]; do
    sleep 1000
done
