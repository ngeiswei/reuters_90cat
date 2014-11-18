#!/bin/bash

set -u

if [[ $# != 4 ]]; then
    echo "Usage: $0 SETTINGS_FILE TRAIN_FILE RND_SEED MOSES_OUTPUT"
    exit 1
fi

#############
# Constants #
#############

PRG_PATH="$(readlink -f "$0")"
PRG_DIR="$(dirname "$PRG_PATH")"
SETTINGS="$1"
TRAIN_FILE="$2"
RND_SEED="$3"
MOSES_OUTPUT="$4"

# Source settings and common.sh
. "$SETTINGS"
. "$PRG_DIR/common.sh"

MOSES_LOG="moses_$(chg_ext "$TRAIN_FILE" log)"

########
# Main #
########

set -x

${MOSES_BUILD}moses \
    -i "$TRAIN_FILE" \
    -r $RND_SEED \
    -j $jobs \
    --min-pool $jobs \
    -H $fitness_type \
    -q $min_recall \
    -Q $alpha \
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
    --enable-fs $(bool2bin $enable_fsm) \
    --fs-target-size $fsm_nfeats \
    --fs-focus all \
    --fs-prune-exemplar 1 \
    --fs-algo $fsm_algo \
    --fs-scorer $fsm_scorer \
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
