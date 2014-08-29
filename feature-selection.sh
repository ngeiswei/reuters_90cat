#!/bin/bash

set -u

if [[ $# != 3 ]]; then
    echo "Usage: $0 SETTINGS_FILE TRAIN_FILE RND_SEED"
    exit 1
fi

#############
# Constants #
#############

PROG_PATH="$(readlink -f "$0")"
PROG_DIR="$(dirname "$PROG_PATH")"
SETTINGS="$1"
TRAIN_FILE="$2"
RND_SEED="$3"

# Source settings and common.sh
. "$SETTINGS"
. "$PROG_DIR/common.sh"

FS_LOG="$(chg_ext "$TRAIN_FILE" log)"
FS_OUTPUT="filtered_$TRAIN_FILE"

########
# Main #
########

set -x

feature-selection \
    -a inc \
    -i "$TRAIN_FILE" \
    -o "$FS_OUTPUT" \
    -r $RND_SEED \
    -C $nfeats \
    -E 0.00001 \
    -D $inc_red_thresh \
    -F "$FS_LOG" \
    -j $jobs \
    -l $log_level

set +x
