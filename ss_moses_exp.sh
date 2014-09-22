#!/bin/bash

set -u
set -x

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

# copy the setting file to the current directory so that the original
# setting file can be modified during the experiment
infoEcho "Copy the settings file to the experiment directory"
if [[ $# == 1 ]]; then
    SRC_SETTINGS="$(readlink -f "$1")"
    DST_SETTINGS="$(basename "$1")"
fi
cp "$SRC_SETTINGS" "$DST_SETTINGS"

# Source settings
. "$DST_SETTINGS"

rnd_seed=$init_rnd_seed
for cat in $categories; do
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

    # Analyze the results. Output a CSV file relating ss_ratio
    # with train and test performances
    PERF_FILE=performances.csv
    header="ss_ratio,training,test"
    echo "$header" > "$PERF_FILE"
    for ss_ratio in ${subsmp_ratios[@]}; do
        exp_dir=ss_ratio_$ss_ratio
        avg_train=$(cat $exp_dir/training.moses | mean)
        avg_test=$(cat $exp_dir/test.moses | mean)
        content="$ss_ratio,$avg_train,$avg_test"
        echo "$content" >> "$PERF_FILE"
    done

    cd ..
done
