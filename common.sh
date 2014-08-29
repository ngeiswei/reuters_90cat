#############
# Functions #
#############

# Given an error message, display that error on stderr and exit
fatalError() {
    echo "[ERROR] $@" 1>&2
    exit 1
}

warnEcho() {
    echo "[WARN] $@"    
}

infoEcho() {
    echo "[INFO] $@"
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