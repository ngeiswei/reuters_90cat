#############
# Functions #
#############

# Output date at a certain format
my_date() {
    date --rfc-3339=seconds
}

# Given an error message, display that error on stderr and exit
fatalError() {
    echo "[$(my_date)] [ERROR] $@" 1>&2
    exit 1
}

warnEcho() {
    echo "[$(my_date)] [WARN] $@"
}

infoEcho() {
    echo "[$(my_date)] [INFO] $@"
}

# Change the extension of a file name. The first argument is the file
# name and the second argument is the extension of replacement.
chg_ext() {
    local filename="$1"
    local extension="${filename##*.}"
    local filename_without_extension="${filename%.*}"
    local new_extension=$2
    echo ${filename_without_extension}.$new_extension
}

# true -> 1
# false -> 0
# Case insensitive
bool2bin() {
    local lc=${1,,}
    if [[ $lc == true ]]; then
        echo 1
    elif [[ $lc == false ]]; then
        echo 0
    else
        fatalError "No boolean constant matching"
    fi
}

# Convert human readable integer into machine full integer. For
# instance $(hr2i 100K) returns 100000, $(hr2i 10M) returns 10000000.
hr2i() {
    local val=$1
    local val=${val/M/000K}
    local val=${val/K/000}
    echo $val
}

# Calculate mean (ignoring nan)
mean() {
    grep -v nan | stats | grep mean | tr -d ' ' | cut -d: -f2
}

#############
# Constants #
#############

# This is a horrible way to regex float, it give way to many false
# positives but let's start with that
readonly float_re='[0-9.Ee+-]+'

# Regex to capture the score components of a composite score output
# 1. non penalized score
# 2. penalized score
# 3. complexity
# 4. complexity penalty
# 5. diversity penalty
readonly composite_score_re="\[score=($float_re), penalized score=($float_re), complexity=([0-9]+), complexity penalty=($float_re), diversity penalty=($float_re)\]"

# In case MOSES isn't install on the system. Don't forgot to end it by
# '/' cause this is gonna be preprended to moses or feature-selection
# without extra '/'
MOSES_BUILD=""
FEATURE_SELECTION_BUILD=""
