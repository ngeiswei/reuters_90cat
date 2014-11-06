#!/bin/bash

set -u
# set -x

if [[ $# != 1 ]]; then
    echo "Usage: $0 SETTINGS_FILE"
    exit 1
fi

#############
# Constants #
#############

PROG_PATH="$(readlink -f "$0")"
PROG_DIR="$(dirname "$PROG_PATH")"
ARCHIVE_DIR="$PROG_DIR/Reuters21578-Apte-90Cat"
SRC_DIR="$PROG_DIR/src"

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

# Source settings
. "$DST_SETTINGS"

rnd_seed=$init_rnd_seed

# Map sample, category and ss_ratio to performance
declare -A moses_perf

for cat in ${categories[@]}; do
    mkdir "$cat"
    cd "$cat"
    if [[ $skip_dataset_generation == false ]]; then
        infoEcho "Parse Reuters90 and create train and test CSV files for $cat"
        "$SRC_DIR/parse_reuters_archive" "$ARCHIVE_DIR" "$cat"
    else
        warnEcho "Skip parse Reuters90"
    fi

    for ss_ratio in ${subsmp_ratios[@]}; do
        SUBSMP_DIR="ss_ratio_$ss_ratio"
        mkdir "$SUBSMP_DIR"
        cd "$SUBSMP_DIR"

        # Subsample the training file
        if [[ $skip_subsampling == false ]]; then
            infoEcho "Subsample training CSV file (subsample ratio = $ss_ratio)"
            "$SRC_DIR/subsample" "../training_$cat.csv" $ss_ratio $rnd_seed
        else
            warnEcho "Skip subsample training CSV file"
        fi

        # Select the most important features
        # Define training file to be passed to feature selection
        TF=training_${cat}_ss_ratio_${ss_ratio}_rnd_seed_${rnd_seed}.csv
        if [[ $skip_feature_selection == false ]]; then
            infoEcho "Run pre-feature selection on $TF"
            "$PROG_DIR/feature-selection.sh" "../../$DST_SETTINGS" \
                "$TF" "$rnd_seed"
        else
            warnEcho "Skip feature selection"
        fi

        # Run MOSES
        # Define filtered training file to be passed to MOSES
        FTF=filtered_$TF
        if [[ $skip_learning == false ]]; then
            infoEcho "Run subsample MOSES on the subsampled training set"
            "$PROG_DIR/moses.sh" "../../$DST_SETTINGS" "$FTF" "$rnd_seed"
        else
            warnEcho "Skip learning"
        fi

        # Evaluate the population on test
        MOSES_OUTPUT=training.moses
        TEST_FILE="../test_$cat.csv"
        infoEcho "Evaluate the model population on test"
        "$PROG_DIR/evaluate.sh" "../../$DST_SETTINGS" "$MOSES_OUTPUT" "$TEST_FILE"

        ((++rnd_seed))

        cd ..
    done

    # Analyze the results for a certain category. Output a CSV file
    # relating ss_ratio with train and test performances
    PERF_FILE=performances.csv
    header="ss_ratio,training,test"
    echo "$header" > "$PERF_FILE"
    for ss_ratio in ${subsmp_ratios[@]}; do
        exp_dir=ss_ratio_$ss_ratio
        moses_perf[train,$cat,$ss_ratio]=$(cat $exp_dir/training.moses | cut -d' ' -f1 | mean)
        moses_perf[test,$cat,$ss_ratio]=$(cat $exp_dir/test.moses | cut -d' ' -f1 | mean)
        content="$ss_ratio,${moses_perf[train,$cat,$ss_ratio]},${moses_perf[test,$cat,$ss_ratio]}"
        echo "$content" >> "$PERF_FILE"
    done

    cd ..
done

# Analyze the results across categories. Output a CSV file, adding
# averaging across categories
PERF_FILE=cross_category_performances.csv
header="category,ss_ratio,training,test"
echo "$header" > "$PERF_FILE"
for cat in ${categories[@]}; do
    for ss_ratio in ${subsmp_ratios[@]}; do
        exp_dir=ss_ratio_$ss_ratio
        content="$cat,$ss_ratio,${moses_perf[train,$cat,$ss_ratio]},${moses_perf[test,$cat,$ss_ratio]}"
        echo "$content" >> "$PERF_FILE"
    done
done
# Add the average across categories
for ss_ratio in ${subsmp_ratios[@]}; do
    cross_cat_train_perf=$(for cat in ${categories[@]}; do echo "${moses_perf[train,$cat,$ss_ratio]}"; done | mean)
    cross_cat_test_perf=$(for cat in ${categories[@]}; do echo "${moses_perf[test,$cat,$ss_ratio]}"; done | mean)
    echo "avg,$ss_ratio,$cross_cat_train_perf,$cross_cat_test_perf" >> "$PERF_FILE"
done
