#!/bin/bash

# External rules for BSPWM.  This script is part of my dotfiles:
# https://github.com/protesilaos/dotfiles.
#
# Copyright (c) 2019-2024  Protesilaos Stavrou <info@protesilaos.com>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.


# NOTE 2021-08-27: Needs review, but I am bringing it back anyway
# because it should still work fine based on what I remember.


# Manual tiling
# =============

# Spawn window on the newest receptacle or preselection and switch focus
# to it.  For multiple presels, priority is given to the current
# desktop.
_bspc_query() {
	bspc query -N -n "$@"
}

recept="$(_bspc_query 'any.leaf.!window')"
presel="$(_bspc_query 'newest.!automatic')"

# Receptacles will not switch focus to the present desktop, whereas
# preselection will.  This way we can develop different workflows (e.g.
# create 3 recept in one desktop, launch 3 GUIs that take time to load,
# switch to another desktop and continue working, until you decide to go
# back to the GUIs).  This has no effect when all actions occur within
# the focused desktop.
#
# Also see my SXHKD bindings for advanced manual tiling actions (refer
# to my dotfiles).
if [ -n "$recept" ]; then
	target="$recept"
	attention="off"
elif [ -n "$presel" ]; then
	target="$presel"
fi

echo "node=${target:-focused}"
echo "follow=${attention:-on}"

# Window rules
# ============

# Operate on windows based on their properties.  The positional
# arguments are defined in the `external_rules_command` of `man bspc`.
window_id="$1"
window_class="$2"
window_instance="$3"
window_title="$(xwininfo -id "$window_id" | sed ' /^xwininfo/!d ; s,.*"\(.*\)".*,\1,')"

# NOTE 2021-09-06: I don't necessarily use those, but it is good to have
# this here.
case "$window_class" in
    [Ee]macs)
        if [ "$window_title" = "prot-window-popup" ]
        then
		    echo "state=floating"
            echo "center=on"
        else
            echo "state=tiled"
        fi
        ;;
	[Ee]o[mg]|[Ff]eh|[Rr]istretto|[Ss]xiv|my_float_window)
		echo "state=floating"
		echo "center=on"
		;;
    [Mm]ate-calc)
		echo "state=pseudo_tiled"
        ;;
	* )
		case "$(xprop -id "$window_id" _NET_WM_WINDOW_TYPE)" in
			*_NET_WM_WINDOW_TYPE_DIALOG*)
				echo "state=floating"
				;;
			*)
                # FIXME 2023-01-23: Those nested `case' need to be
                # simplified.  I keep nesting them here because I am
                # in a hurry...  This specific case covers the small
                # floating window that is produced by Jitsi while
                # sharing the screen.
                case "$(xprop -id "$window_id" _NET_WM_STATE)" in
                    *_NET_WM_STATE_STICKY*|*_NET_WM_STATE_ABOVE*)
                        echo "state=floating"
                        ;;
                    *)
				        echo "state=tiled"
				        ;;
                esac
		esac
		;;
esac

# FIXME the "file operations" applies to the `caja` file manager.
# TODO There should be a better way of handling this.
case "$window_title" in
	'File Operations'*)
		echo "state=floating"
		echo "center=on"
		;;
	my_float_window)
		echo "state=floating"
		;;
esac
