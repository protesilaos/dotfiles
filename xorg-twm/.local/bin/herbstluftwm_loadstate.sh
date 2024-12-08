#!/usr/bin/env bash

# PROT 2024-12-08: I got this from the example scripts that are part
# of the herbstluft package on Debian stable.

hc() { "${herbstclient_command[@]:-herbstclient}" "$@" ;}

# loads layouts for each tag coming from stdin
# the format is the one created by savestate.sh

# a common usage is:
# savestate.sh > mystate
# and sometime later:
# loadstate.sh < mystate

while read line ; do
    tag="${line%%: *}"
    tree="${line#*: }"
    hc add "$tag"
    hc load "$tag" "$tree"
done
