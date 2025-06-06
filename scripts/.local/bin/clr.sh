#!/bin/bash

# CLR --- Check Luminance Ratio between two colours.
# Implements the formula provided by the Web Content Accessibility
# Guidelines (WCAG) 2.0: https://www.w3.org/TR/WCAG20-TECHS/G18.html
#
# CLR is part of my dotfiles: https://github.com/protesilaos/dotfiles.
# Apart from its utility, this script has been developed as a means of
# practicing certain shell scripting techniques.
#
# For my accessible colour schemes (which comply with the WCAG
# guidelines), refer to my Tempus themes (partially incorporated in my
# dotfiles as well): https://gitlab.com/protesilaos/tempus-themes.
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
# Debian Buster dependency:
# 	apt install apcalc
# The executable is `calc`.

# NOTE this is a working prototype
# TODO add inline documentation
# TODO code review

[ "$(command -v calc 2> /dev/null)" ] || { echo "Missing calc dependency"; exit 1; }
[ "$#" -eq 2 ] || { echo "Must pass two arguments, representing valid HEX colours"; exit 1; }

# This uses Bash-specific syntax
_valid_hex() {
	[[ $1 =~ ^[0-9A-Fa-f]*$ ]] || { echo "$1 needs to be a valid HEX colour"; exit 1; }
}

_valid_hex "$1"
_valid_hex "$2"

_six_digits() {
	local arg
	arg="$1"

	if [ "${#arg}" -eq 3 ]; then
		printf "%s" "${arg:0:1}${arg:0:1}${arg:1:1}${arg:1:1}${arg:2:1}${arg:2:1}"
	elif [ "${#arg}" -eq 6 ]; then
		echo "$arg"
	fi
}

for i in "$@"; do
	case "${#i}" in
		3|6)
			foreground="$(_six_digits "$1")"
			background="$(_six_digits "$2")"
			continue
			;;
		*)
			echo "$i is not a valid colour."
			exit 1
			;;
	esac
done

hex_to_rgb() {
	printf '%d,%d,%d' 0x"${1:0:2}" 0x"${1:2:2}" 0x"${1:4:2}"
}

# Convert the colours into R/G/B
foreground="$(hex_to_rgb "$foreground")"
background="$(hex_to_rgb "$background")"

# Get the value of each of the R/G/B channels using just parameter
# expansion.  NOTE that the green channel requires two operations to be
# retrieved.
fg_red="${foreground/,*}"
fg_green="${foreground%,*}"
fg_green="${fg_green#*,}"
fg_blue="${foreground##*,}"
bg_red="${background/,*}"
bg_green="${background%,*}"
bg_green="${bg_green#*,}"
bg_blue="${background##*,}"

_rgb_range() {
	local channel
	channel="$(calc -dp "$1/255")"

	if [ "$(calc -dp "$channel<=0.03928")" -eq 1 ]; then
		channel="$(calc -dp "($channel/12.92)")"
		echo "$channel"
	else
		channel="$(calc -dp "((${channel}+0.055)/1.055)^2.4")"
		echo "$channel"
	fi
}

fg_red="$(_rgb_range "$fg_red")"
fg_green="$(_rgb_range "$fg_green")"
fg_blue="$(_rgb_range "$fg_blue")"
bg_red="$(_rgb_range "$bg_red")"
bg_green="$(_rgb_range "$bg_green")"
bg_blue="$(_rgb_range "$bg_blue")"

_find_l() {
	calc -dp "(0.2126*$1)+(0.7152*$2)+(0.0722*$3)"
}

foreground="$(_find_l "$fg_red" "$fg_green" "$fg_blue")"
background="$(_find_l "$bg_red" "$bg_green" "$bg_blue")"

_ratio() {
	local light dark

	if [ "$(calc -dp "$1>$2")" -eq 1 ]; then
		light="$foreground"
		dark="$background"
	else
		light="$background"
		dark="$foreground"
	fi

	calc -dp "(${light}+0.05)/(${dark}+0.05)"
}

result="$(_ratio "$foreground" "$background")"

echo "${result:0:4}:1"
