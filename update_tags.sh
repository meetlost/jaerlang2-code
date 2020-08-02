#!/bin/sh

CWD=`pwd`

# Produce ProjectTAGS.
find -L ${PWD} -name "*.[he]rl" | etags -o TAGS -
