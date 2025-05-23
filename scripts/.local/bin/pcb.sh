#!/bin/bash

# pcb --- Print the terminal's active colours.
#
# Copyright (c) 2019-2025  Protesilaos Stavrou <info@protesilaos.com>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.
#
## Commentary:
#
# Print a table with the active terminal's 16 colours.  The presenation
# is made in two rows, while trying to be aware of the width of the
# terminal.
#
# Modified from source:
# http://bitmote.com/index.php?post/2012/11/19/Using-ANSI-Color-Codes-to-Colorize-Your-Bash-Prompt-on-Linux
# Under under the terms of CC BY license.


# Make the size of the blocks dependent on the width of the terminal.
columns="$(tput cols)"
if [ "$columns" -gt 90 ]; then
	space='         '           # 9
elif [ "$columns" -gt 80 ]; then
	space='      '              # 6
else
	space='   '                 # 3
fi

echo -e "\n"

# Run through the 16 colours.  Create two rows.
for i in {0..15}; do
    echo -en "\033[48;5;${i}m${space}\033[m "
        if [ "$i" = 7 ]; then
            echo '' && continue
        fi
done

echo -e "\n"
