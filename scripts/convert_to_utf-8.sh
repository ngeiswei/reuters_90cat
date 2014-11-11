#!/bin/bash

# Convert all text files to utf-8
find Reuters21578-Apte-90Cat -type f -exec iconv {} -t utf-8 -o {} \;
