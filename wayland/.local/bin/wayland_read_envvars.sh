#!/bin/bash

#  NOTE 2024-04-08: Do not use this. It is experimental.

while read -r line
do
    eval export $line
done < <(cat "$HOME"/.config/environment.d/envvars.conf)
