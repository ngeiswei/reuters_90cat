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

MOSES_LOG="$(chg_ext "$TRAIN_FILE" log)"
MOSES_OUTPUT=training.moses

########
# Main #
########

set -x

moses \
    -i "$TRAIN_FILE" \
    -r $RND_SEED \
    -j $jobs \
    --min-pool $jobs \
    -H $fitness_type \
    --complexity-ratio $complexity_ratio \
    -m $(hr2i $evals) \
    -M $candidates_per_deme \
    --cap-coef $metapop_cap_coef \
    --hc-fraction-of-nn $fraction_of_nn \
    --complexity-temperature $complexity_temperature \
    --hc-max-nn-evals $max_nn_evals \
    --hc-allow-resize-deme 0 \
    -Z $(bool2bin $crossover) \
    --hc-crossover-pop-size $crossover_pop_size \
    --hc-crossover-min-neighbors $crossover_min_neighbors \
    -T $(bool2bin $expand_search) \
    -D $expand_dst \
    -B $rep_reduct_effort \
    --revisit $revisit \
    --ss-n-subsample-demes $ss_demes \
    --ss-n-top-candidates $ss_top_candidates \
    --ss-tanimoto-geometric-mean-threshold $ss_tanimoto_geometric_mean_threshold \
    -o "$MOSES_OUTPUT" \
    -c $max_candidates \
    -W 1 \
    -x 1 \
    -S 1 \
    -t 1 \
    --output-deme-id 1 \
    -f "$MOSES_LOG" \
    -l $log_level \
    -s $(hr2i $cache_size)

set +x
