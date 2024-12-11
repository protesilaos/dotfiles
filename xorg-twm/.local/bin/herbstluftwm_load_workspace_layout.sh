#!/bin/bash

# herbstluftwm_load_workspace_layout --- Load preset layout in the current workspace
#
# Copyright (c) 2024  Protesilaos Stavrou <info@protesilaos.com>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.	If not, see <https://www.gnu.org/licenses/>.

my_loaded_layout=""

case $1 in
    third)
        herbstclient load \
                     "(split horizontal:0.25:0.75 (clients vertical:0) (clients max:0))" \
            && my_loaded_layout="third"

        ;;
    half)
        herbstclient load \
                     "(split horizontal:0.5:0.5 (clients max:0) (clients max:0))" \
            && my_loaded_layout="half"
        ;;
    middle)
        herbstclient load \
                     "(split horizontal:0.2:0.8 (clients vertical:0) (split horizontal:0.75:0.25 (clients max:0) (clients vertical:0)))" \
            && my_loaded_layout="middle"
        ;;
esac

notify-send -i desktop "HerbstluftWM workspace layout" "Loaded the ${my_loaded_layout^^} layout"
