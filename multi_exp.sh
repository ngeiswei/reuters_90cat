#!/bin/bash

# Perform a series of subsampling experiments varying some parameters

if [[ $# != 1 ]]; then
    echo "Usage: $0 SETTINGS_FILE"
    exit 1
fi

set -u
set -x

#############
# Constants #
#############

PROG_PATH="$(readlink -f "$0")"
PROG_DIR="$(dirname "$PROG_PATH")"

# Name of the parameter to vary
variable_parameter=ss_tanimoto_geometric_mean_threshold

# Values of the parameter to vary
# values=(0.{1..9} 1)
values=(0.5 1)

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
cross_exp_file=cross_exp_performances.csv
header="$variable_parameter,category,ss_ratio,training,test"
echo "$header" > "$cross_exp_file"
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

    # Cross experiments analysis
    exp_perf_file="$exp_dir/cross_category_performances.csv"
    N=$(($(wc -l < $exp_perf_file) - 1))
    paste -d, <(printf "$v\n%.0s" $(seq 1 $N)) <(tail -n +2 $exp_perf_file) \
        >> $cross_exp_file
done

# # Plot the results
# for v in ${values[@]}; do
    
# done
