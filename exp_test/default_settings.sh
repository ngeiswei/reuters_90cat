# Settings controlling a subsampling MOSES experiment

###################
# Skip parameters #
###################

# Those flags allow to skip parts of the pipeline. Note that for a
# given experiments you must have enabled it at least once in order to
# proceed to the next step. For instance learning cannot take place if
# the files output by feature selection aren't there, and feature
# selection cannot take place if the training data files aren't
# there.

# Skip dataset generation
skip_dataset_generation=true

# Skip subsampling
skip_subsampling=true

# Skip feature selection
skip_feature_selection=true

# Skip MOSES learning
skip_learning=false

#########################
# Experiment parameters #
#########################

# Initial random see (then the random is incremented for each
# experiment of the series)
init_rnd_seed=1

# number of jobs
jobs=4

# Maximum number of candidates output by MOSES
max_candidates=30

# Log level: info, debug (default) or fine
log_level=debug

###############################
# Fitness function parameters #
###############################

# Fitness function type. Supported types are
#
# f_one
fitness_type=f_one

####################################
# Pre-feature selection parameters #
####################################

# Number of features to be selected
nfeats=500

# Parameters for inc feature selection algorithm. Level of redundancy
# to discard, 0 means all redundant features are kept, 1 means even
# the slightliest redudant features are removed. That value does not
# change the number of features being selected. A value of 0 will make
# inc feature selection much faster but will retain all redundant
# features.
inc_red_thresh=0

###########################
# MOSES search parameters #
###########################

# Number of evaluations . You can use K for 1000 and M for 1000000, so
# for instance 2M will be interpreted as 2000000.
evals=10K

# Number of candidates the cache can hold. The larger the cache the
# faster but the more memory it requires. Like for eval you can use K
# and M.
cache_size=500K

# The number of candidates to keep after searching a deme, a low
# number will decrease memory and speed up merging, but a too low
# number will discard too many iteresting candidates.
candidates_per_deme=500

# Complexity ratio (lower means higher complexity penalty)
complexity_ratio=1500

# Boltzmann temperature of the probability distribution to chose the
# next exemplar to generate a deme to search. The right value depends
# on the fitness landscape, a very deceptive fitness landscape
# requires a higher value, while a monotonic one requires a low
# value. You need a stricly positive number (temperature zero will
# crash moses).
#
# Also, the temperature is used to calculate the score below which
# candidates are discarded from the metapopulation. So if too few
# candidates are returned by MOSES, it might be because the
# temperature is too low. To understand whether that is the problem,
# look for the strings
#
# "Trim down deme"
#
# and
#
# "Deme trimmed down, new size"
#
# in MOSES log and see if the new size is systematically too small,
# increase the temperature if that is the case.
complexity_temperature=6

# Another way to control the size of the metapopulation (therefore
# decreasing memory usage). Acceptable values are strictly positive
# float. A low value will speed-up diversity computation as well, but
# a too low value may discard interesting candidates.
metapop_cap_coef=1

# Control the fraction of nearest neighborhood to explore. Note that
# it is only an approximation as the number neighbors is approximated
# too
fraction_of_nn=0.5

# Control the size of a deme by setting a cap on the number of
# neighbors to search from a given distance to the exemplar. This is
# useful when the number of knobs is very large to limitate the size
# of the deme.
max_nn_evals=10000

# cheap way to combine several knobs at once
crossover=true

# The number of candidates to consider when doing crossover.
crossover_pop_size=300

# Threshold to control when crossover is triggered. If the number of
# neighbors to explore is greater than the threshold (and at least 2
# iterations have already passed) then crossover kicks in.
crossover_min_neighbors=300

# In addition to crossover one explore at further distance (turn
# several knobs at once), that can be very expensive though in term of
# number of evals.
expand_search=true

# if expand_search is true, then what is the maximum distance to expand to
expand_dst=2

# perm_ratio (between 0 and 1), ratio of binary permutations used
# during representation building (0 means arity permutation, 1 means
# arity*(arity-1)/2 permutation).
perm_ratio=0

# Reduct effort during representation building (0 to 3)
rep_reduct_effort=2

# Number of times the same exemplar can be revisited (0 means it is
# visited only once)
revisit=0

################################
# Subsampling-MOSES parameters #
################################

# Number of ss-demes (if 0 or 1, then it is disabled)
ss_demes=0

# Number of top candidates to consider for each ss-deme
ss_top_candidates=1

# Tanimoto geometric mean threshold. In order to be merged into the
# metapopulation the average pairwise tanimoto distance between the
# top candidates of the subsampled demes must be below that
# threshold
ss_tanimoto_geometric_mean_threshold=1

####################################
# Subsampling Reuters90 parameters #
####################################

# List of categories to classify
categories=(cocoa)

# Sequence of subsampling ratios to apply on the training dataset. The
# lower the subsampling ratio, the higher the propensity to overfit.
subsmp_ratios=(0.{1..9})
