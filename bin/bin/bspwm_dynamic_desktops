#!/bin/bash

# Dynamic workspaces for BSPWM.  Switching to a non-existent desktop
# will create it dynamically.  Same with sending nodes to one.  Empty
# desktops are removed.  This is meant to be implemented in SXHKD.  Part
# of my dotfiles: https://gitlab.com/protesilaos/dotfiles
#
# Copyright (c) 2019 Protesilaos Stavrou <info@protesilaos.com>
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
# along with this program.  If not, see <http://www.gnu.org/licenses/>.


# See the --options below for the available arguments for $1.  $2 must
# be a number from 0 to 9.
[ "$#" -eq 2 ] || { echo 'Must be run with two arguments.'; exit 1; }

# Capture the arguments.  We need to run tests against them.  Renaming
# them makes things easier to read.
option="${@:1:1}"
target_desktop="${@:2:1}"

case "$target_desktop" in
	[!0-9])
		echo "The second argument must be a valid number"
		exit 1
		;;
esac

# Command for querying BSPWM desktops.
_query_desktops() {
	bspc query -D -d "$@"
}

# The core functionality of this script.  Behaviour changes based on
# whether this is a desktop or a node (each action is mapped to
# different key chords in sxhkd).
#
# If the target desktop does not exist, it is created on the spot.
_desk_or_node() {
	_dynamic_desktops() {
		# Here we make sure we clean up the arguments passed to this
		# function.  Then we can operate on them with greater
		# flexibility.
		local monitor action
		monitor="${@:1:1}"
		action="${@:2}"

		# Leave the unquoted in tact!  Else the commands will not run.
		if ! _query_desktops "$target_desktop" > /dev/null; then
			bspc monitor "$monitor" -a "$target_desktop" && $action
		else
			# BACK-AND-FORTH behaviour:  inputting the number of the
			# focused desktop switches to the last one.
			#
			# If you do not like this, just replace the whole if/fi part
			# below with $action (and do not quote it).
			if [ "$(_query_desktops --names)" -eq "$target_desktop" ]; then
				bspc desktop -f last
			else
				$action
			fi
		fi
	}

	# Reorder desktops on the target monitor (the target is the first
	# argument passed to this function.  The default is the focused one.
	_desk_order() {
		while read -r line; do
			printf "%s\\n" "$line"
		done < <(bspc query -D -m "${1:-focused}" --names) | sort -g | paste -d ' ' -s
	}

	# Determines whether the behaviour of this function concerns nodes
	# or desktops.  The "monitor" is just a filter for nodes/desktops,
	# hence the "shift".  It concerns multihead setups.
	case "$1" in
		node|desktop)
			_dynamic_desktops 'focused.focused' bspc "$@"
			bspc monitor -o $(eval _desk_order) # do not quote! we want term splitting here
			;;
		monitor)
			shift
			_dynamic_desktops next bspc "$@"
			bspc monitor next -o $(eval _desk_order next) # do not quote! we want term splitting here
			;;
	esac
}

# Invoke the above command, passing to it arguments that change its
# behaviour (desktop or node).
#
# Mnemonics for shorter options:
# n* == node
# d* == desktop
#
# TODO LOWPRIORITY --no|--send-all-to-desktop-next-monitor*)
# TODO to be implemented in bspwm_multifaceted_operations and sxhkd
case "$option" in
	--na|--send-all-to-desktop)
		_desk_or_node node 'any.local' -d "${target_desktop}"
		;;
	--ns|--send-focused-to-desktop)
		_desk_or_node node focused -d "${target_desktop}"
		;;
	--nm|--send-focused-to-next-monitor)
		_desk_or_node monitor node focused -d "${target_desktop}"
		;;
	--da|--activate-desktop-and-place-receptacle)
		_desk_or_node monitor node @"$target_desktop":/ -i && bspc desktop -a "${target_desktop}"
		;;
	--df|--focus-desktop)
		_desk_or_node desktop -f "${target_desktop}"
		;;
	*)
		echo "< $option > is not a valid option."
		exit 1
		;;
esac

# Remove empty desktops.  This works for multiple monitors, as well.
# This will NOT REMOVE empty desktops that contain only receptacles
# (applies to the --activate-desktop option above).
for i in $(_query_desktops '.!focused.!occupied' --names); do
	bspc desktop "$i" -r
done
