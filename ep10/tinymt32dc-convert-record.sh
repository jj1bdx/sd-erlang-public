#!/bin/sh
grep -v "^#" tinymt32dc.0.65535.txt | \
    awk 'BEGIN{FS=",";}{print "tinymt_params:add_record(#tinymt32param{characteristic=16#" $1 ",mat1=16#" $4 ",mat2=16#" $5 ",tmat=16#" $6 ",weight=" $7 ",delta=" $8 "}),";}' > tinymt32dc-tuples.txt
