# xinitrc --- Initialisation script for the X server session.
#
# Copyright (c) 2019-2022 Protesilaos Stavrou <info@protesilaos.com>
#
# This program is free software; you can redistribute it and/or
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
#
### Commentary:
#
# This script is a core part of my custom desktop session, centred
# around GNU Emacs: <https://gitlab.com/protesilaos/dotfiles>.
#
### Code:


#### Shell setup

# Source my bashrc.
if [ -n "$BASH_VERSION" ]; then
    if [ -f "$HOME"/.bashrc ]; then
        source "$HOME"/.bashrc
    fi
fi

# Set PATH so it includes user's private executables.
if [ -d "$HOME"/bin ]; then
    PATH=$PATH:"$HOME"/bin
fi

# This is another possible location for user-specific binaries.
if [ -d "$HOME"/.local/bin ]; then
    PATH=$PATH:"$HOME"/.local/bin
fi

#### Environment essentials

# Auto unlocks the GPG and SSH agents.
eval $(/usr/bin/gnome-keyring-daemon --start --components=pkcs11,secrets,ssh)
export SSH_AUTH_SOCK

#### Session selection

# Launch X by running:
#
#   xinit
#   xinit xfce
#
# Or pass this file and the DE/WM arg to startx:
#   startx ~/.xinitrc emacs
#
# Also check my `.bash_profile' for the automatic login setup.

# Emacs is the default.
session=${1:-bspwm}

#### Sessions

# Map arguments to commands.
case $session in
    bspwm ) exec bspwm        ;;
    mate  ) exec mate-session ;;
    xfce  ) exec startxfce4   ;;
    *) echo "Unknown session" ;;
esac
