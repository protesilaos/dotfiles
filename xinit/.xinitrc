# xinitrc --- Initialisation script for Void Linux.
#
# Copyright (c) 2019 Protesilaos Stavrou <info@protesilaos.com>
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
## Commentary:
#
# This script is a core part of my custom desktop session, centred
# around BSPWM.  My setup runs on Void Linux and is managed with GNU
# Stow.  The underlying OS is meant to remain in a minimal, clean state.
# My main interface for most computing tasks is GNU Emacs.  Refer to my
# dotfiles for the specifics: https://gitlab.com/protesilaos/dotfiles


###############
# Shell setup #
###############

# Source my bashrc.
if [ -n "$BASH_VERSION" ]; then
    if [ -f "$HOME/.bashrc" ]; then
        source "$HOME/.bashrc"
    fi
fi

# Set PATH so it includes user's private executables.
#
# TODO 2019-11-12: Consider pros/cons of splitting scripts at ~/scripts
# from binaries at ~/bin.
if [ -d "$HOME/bin" ]; then
    PATH=$PATH:"$HOME"/bin
fi

# NOTE 2019-11-12: This is another possible location for user-specific
# binaries.  It might actually be better to merge everything here
# (without prejudice to the above TODO).
if [ -d "$HOME/.local/bin" ]; then
    PATH=$PATH:"$HOME"/.local/bin
fi

##########################
# Environment essentials #
##########################

# Auto unlocks the GPG and SSH agents.  Note the specific syntax for
# these, as they differ from what is available on the Void Linux Wiki.
export $(dbus-launch)
export $(gnome-keyring-daemon --start)

#####################
# Session selection #
#####################

# Launch X by running:
# 	xinit
#   xinit xfce
#
# Or pass this file and the DE/WM arg to startx:
#   startx ~/.xinitrc bspwm

# BSPWM is the default.
session=${1:-bspwm}

# Map arguments to commands.
case $session in
    bspwm ) exec bspwm        ;;
    mate  ) exec mate-session ;;
    xfce  ) exec startxfce4   ;;
    *) echo "Unknown session" ;;
esac
