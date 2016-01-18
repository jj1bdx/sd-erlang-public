#!/bin/sh
grep -v "^#" tinymt32dc.0.65535.txt | \
    awk 'BEGIN{FS=",";}{print "{tinymt32param, 16#" $1 ", 16#" $4 ", 16#" $5 ", 16#" $6 ", " $7 ", " $8 "}.";}' > tinymt32dc-rawtuples.txt
