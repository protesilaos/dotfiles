#!/bin/bash

    # This program is free software: you can redistribute it and/or modify
    # it under the terms of the GNU General Public License as published by
    # the Free Software Foundation, either version 3 of the License, or
    # (at your option) any later version.

    # This program is distributed in the hope that it will be useful,
    # but WITHOUT ANY WARRANTY; without even the implied warranty of
    # MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    # GNU General Public License for more details.

    # You should have received a copy of the GNU General Public License
    # along with this program.  If not, see <http://www.gnu.org/licenses/>.

# I use a custom font with my `dmenu` interfaces.  The current file
# defines as much.  This file is sourced from some of my scripts at
# $HOME/bin.  The idea is to define the custom font once.  This script is
# part of my dotfiles and is meant to work in that environment.  See
# https://gitlab.com/protesilaos/dotfiles

# For the actual font family, see the rules inside the "fontconfig"
# directory of my dotfiles.
#
# Desirable values for the font family are the generic `fontconfig`
# aliases: sans, serif, monospace.  The font weight can only be one that
# is supported by the designated font.

# If Terminus is installed, use it (because it is my default monospaced
# typeface in the fontconfig rules).  Else try Monoid.  Otherwise
# default to whatever the sans-serif font is.
# NOTE that we do not define the actual font, but only the fontconfig
# alias.  This is to control the typefaces that are used from a single
# place, rather than on a per-application basis.
if [ -n "$(fc-list terminus)" ]; then
	my_font_family=monospace
	my_font_size=12
elif [ -n "$(fc-list monoid)" ]; then
	my_font_family=monospace
	my_font_size=9
else
	my_font_family=sans
	my_font_size=10
fi

my_font_weight=regular
my_custom_ui_font="$my_font_family-$my_font_size:$my_font_weight"
