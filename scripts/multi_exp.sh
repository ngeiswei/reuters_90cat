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

PRG_PATH="$(readlink -f "$0")"
PRG_DIR="$(dirname "$PRG_PATH")"

# Name of the parameter to vary
variable_parameter=ss_tanimoto_geometric_mean_threshold

# Values of the parameter to vary
# values=(0.{1..9} 1)
values=(0.1 1)

########
# Main #
########

# Source common contants and functions
. "$PRG_DIR/common.sh"

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
header="$variable_parameter,category,ss_train_ratio,training,test"
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
    $PRG_DIR/ss_moses_exp.sh "../$DST_SETTINGS"
    cd ..

    # Cross experiments analysis
    exp_perf_file="$exp_dir/cross_category_performances.csv"
    N=$(($(wc -l < $exp_perf_file) - 1))
    paste -d, <(printf "$v\n%.0s" $(seq 1 $N)) <(tail -n +2 $exp_perf_file) \
        >> $cross_exp_file
done

##############################
# Cross experiments analysis #
##############################

infoEcho "Plot the results"

# Create dat files for gnuplot
declare -A file_for_plot
for v in ${values[@]}; do
    exp_dir=${variable_parameter}_$v

    file_for_plot[$v]=${variable_parameter}_${v}.dat
    grep avg $exp_dir/cross_category_performances.csv | column -s',' -t \
        > ${file_for_plot[$v]}
done

# Create plot scripts and plots
for smp in train test; do
    if [[ $smp == train ]]; then
        column=3
    else
        column=4
    fi
    cat <<EOF > "$smp.gnuplot"
set terminal pngcairo size 800,600 noenhanced font 'Verdana,10'
set output "plot_${smp}.png"
set title "Performance on $smp w.r.t. subsample training ratio"
set xlabel "Subsample training ratio"
set ylabel "Performance"
EOF
    PLOT_CMD="plot"
    for v in ${values[@]}; do
        PLOT_CMD+=" \"${file_for_plot[$v]}\" u 2:$column t \"$variable_parameter=$v\" w lines,"
    done
    echo "$PLOT_CMD" >> "$smp.gnuplot"

    # Plot
    gnuplot "$smp.gnuplot"
done
