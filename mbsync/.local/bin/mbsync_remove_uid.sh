#!/bin/bash

# mbsync_remove_uid.sh --- Remove the UID from email files.
#
# Copyright (C) 2025-2026  Protesilaos Stavrou <info@protesilaos.com>
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
### Commentary:
#
# Remove the mbsync UID from email files. THIS IS RISKY SO PLEASE MAKE
# SURE YOU UNDERSTAND WHAT YOU ARE DOING.
#
# Part of my dotfiles: <https://github.com/protesilaos/dotfiles>.

### Code:

my_mail_directory="$HOME"/.mail

while IFS= read -r -d $'\0' file
do
    if [ -f "$file" ]
    then
        mv "$file" "${file//,U=[0-9]*:/:}"
    fi
done < <(find "$my_mail_directory" -type f -iname "*,U=[0-9]*" -print0)
