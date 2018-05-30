#!/usr/bin/env bash
set -euo pipefail
IFS=$'\n\t'

#/ Usage: ./setup_symlinks.sh
#/ Description: Creates symlinks between the shiny server at /srv/shiny-server
#/ Options:
#/   --help: Display this help message
usage() { grep '^#/' "$0" | cut -c4- ; exit 0 ; }
expr "$*" : ".*--help" > /dev/null && usage

readonly LOG_FILE="/tmp/$(basename "$0").log"
info()    { echo "[INFO]    $*" | tee -a "$LOG_FILE" >&2 ; }
warning() { echo "[WARNING] $*" | tee -a "$LOG_FILE" >&2 ; }
error()   { echo "[ERROR]   $*" | tee -a "$LOG_FILE" >&2 ; }
fatal()   { echo "[FATAL]   $*" | tee -a "$LOG_FILE" >&2 ; exit 1 ; }


# ---- PARAMS ----
ORIGIN_DIR=${PWD}/
SERVER_DIR="/srv/shiny-server/"


clean_up() {
  ARG=$?
  cd ${ORIGIN_DIR}
  exit $ARG
}
trap clean_up EXIT

if [ -d ${SERVER_DIR} ]; then
    cd ${SERVER_DIR}
else
    fatal "directory ${SERVER_DIR} does not exist"
fi


# ---- MAIN ----
info "Linking html files from ${ORIGIN_DIR} to ${SERVER_DIR}:"
info $(ls ${ORIGIN_DIR}*.html)


check_n_link() {
  [ -L "$SERVER_DIR""$1" ] && warning "Symlink already exists at $1"
  [ -L "$SERVER_DIR""$1" ] || ln -s "${ORIGIN_DIR}""$1" "${SERVER_DIR}"/"$1"
}

check_n_link visualizations.html
check_n_link index.html
check_n_link source.html
check_n_link help.html
check_n_link disclaimer.html

