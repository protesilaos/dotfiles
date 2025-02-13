#!/bin/bash

# tmr --- Bash script to run a simple timer
#
# Copyright (c) 2019-2025 Protesilaos Stavrou <info@protesilaos.com>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or (at
# your option) any later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.
#
## Commentary:
#
# TMR Must Recur (pronounced "timer") is a simple command-line timer
# that accepts a single argument that represents a valid unit of time.
#
# Distributed as part of my dotfiles, since I use it daily, though it
# technically is not an integral part of my custom desktop session:
# https://github.com/protesilaos/dotfiles.
#
# TMR dependencies:
# 	mpv libnotify (libnotify-bin) sound-theme-freedesktop


# Define a help message
_help_message() {
    echo "ERROR. Must pass a single argument."
    echo "Examples of valid time units: 10s, 5m, 2h"
    echo "A number without a unit is read as NUM minutes."
}

# Show the help message if arguments are not equal to one.
[ "$#" -eq 1 ] || { _help_message; exit 1; }

# Convert the argument into a human-readable string: to calculate the
# difference between the time this script starts and ends.
arg="$1"
case "$arg" in
    *s) past="${arg%?} seconds ago" ;;
    *m) past="${arg%?} minutes ago" ;;
    *h) past="${arg%?} hours ago"   ;;
    *d) echo "Days? TMR Made Ridiculous!" && exit 1  ;;
    *)  arg="${arg}m" ; past="${arg%?} minutes ago" ;;
esac

time_set="$(date --date="$past" +%T)"
time_now="$(date +%T)"
time_after="$(date --date="${past% ago}" +%T)"
alarm_sound="/usr/share/sounds/freedesktop/stereo/alarm-clock-elapsed.oga"

# Function to check if optional dependencies are in the PATH
_depcheck() {
    command -v "$1" 2> /dev/null
}

# The message to print when the timer is done.  This will echo a message
# in the terminal and, if optional dependencies are present, set a
# desktop notification while ringing the alarm bell for a short time.
_notify() {
    message="tmr elapsed at $time_after (started $past)"

    echo "$message"

    [ "$(_depcheck notify-send)" ] && notify-send -i clock "$message"

    if [ -n "$alarm_sound" ] && [ "$(_depcheck mpv)" ]; then
        mpv "$alarm_sound"
    else
        echo -e "\a"
    fi
}

# Set the timer and send notification when its done
echo "tmr started at $time_now"
sleep "$arg" && _notify
