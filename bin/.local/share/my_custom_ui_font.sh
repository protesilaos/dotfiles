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

# For the actual font family, see my `fonts.conf`, under the "fonts"
# directory of my dotfiles.  Possible values for the font family are the
# generic `fontconfig` aliases: sans, serif, monospace.  The font weight
# can only be one that is supported by the designated font.
my_font_family=sans
my_font_size=9
my_font_weight=regular

my_custom_ui_font="$my_font_family-$my_font_size:$my_font_weight"
