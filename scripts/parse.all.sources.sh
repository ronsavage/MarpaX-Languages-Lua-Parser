#!/bin/bash
#
# This generates all output, which is then used for testing.

for i in lua.sources/* ;
do
	BASE=`basename $i .lua`
	perl -Ilib scripts/parse.file.pl -i lua.sources/$BASE.lua -max debug > lua.output/$BASE.txt
done
