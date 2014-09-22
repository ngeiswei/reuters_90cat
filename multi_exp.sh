#!/bin/bash

# Perform a series of subsampling experiments varying some parameters

if [[ $# != 1 ]]; then
    echo "Usage: $0 SETTINGS_FILE"
    exit 1
fi

set -u
# set -x

#############
# Constants #
#############

PROG_PATH="$(readlink -f "$0")"
PROG_DIR="$(dirname "$PROG_PATH")"

# Name of the parameter to vary
variable_parameter=ss_tanimoto_geometric_mean_threshold

# Values of the parameter to vary
values=(0.{1..9} 1)

########
# Main #
########

# Source common contants and functions
. "$PROG_DIR/common.sh"

# Copy the setting file to the current directory so that the original
# setting file can be modified during the experiment
if [[ $# == 1 ]]; then
    SRC_SETTINGS="$(readlink -f "$1")"
    DST_SETTINGS="$(basename "$1")"
fi
infoEcho "Copy the settings file $SRC_SETTINGS to current directory"
cp "$SRC_SETTINGS" "$DST_SETTINGS"

# Run experiment series
for v in ${values[@]}; do
    infoEcho "Build and run experiment $variable_parameter = $v"

    # Create experiment dir
    exp_dir=${variable_parameter}_$v
    mkdir $exp_dir
    cd $exp_dir

    # Edit settings
    sed -i.bak "s/^$variable_parameter=.\+\$/$variable_parameter=$v/" "../$DST_SETTINGS"

    # Run subsampling experiments
    $PROG_DIR/ss_moses_exp.sh "../$DST_SETTINGS"
    cd ..
done
