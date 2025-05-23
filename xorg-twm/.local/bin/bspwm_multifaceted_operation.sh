#!/bin/bash

# Facilitates BSPWM operations over multiple selection paths.  The
# actions defined herein are assigned to key chords (sxhkd).  Everything
# is part of my dotfiles: https://github.com/protesilaos/dotfiles.
#
# Copyright (c) 2019-2024  Protesilaos Stavrou <info@protesilaos.com>
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

_query_nodes() {
	for i in $(bspc query -N -n "$1"); do
		bspc node "$i" "${@:2}"
	done
}

# Mnemonics for shorter options:
# r* == receptacles
# n* == nodes
# d* == desktops
case "$1" in
	--rk|--kill-all-receptacles)
		_query_nodes '.leaf.!window' -k
		;;
	--nca|--close-all-local-non-focused)
		_query_nodes '.!focused.!marked.local.window' -c
		;;
	--nka|--kill-all-local-non-focused)
		_query_nodes '.!focused.!marked.local.window' -k
		;;
	--nla|--lock-all-local)
		_query_nodes '.local.window' -g locked=on
		;;
	--nula|--unlock-all-local)
		_query_nodes '.local.window' -g locked=off
		;;
	--nms|--summon-all-marked)
		_query_nodes '.marked' -d focused --follow
		;;
	--db|--bring-desktop-here)
		for i in $(bspc query -N -d "$2" -n '.window'); do
			bspc node "$i" -d 'focused.focused'
		done
		;;
	--ds|--send-desktop-there)
		# NOTE another script of mine (see dotfiles).
		bspwm_dynamic_desktops.sh --send-all-to-desktop "$2"
		;;
esac
