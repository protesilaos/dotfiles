# xinitrc --- Initialisation script for the X server session.
#
# Copyright (c) 2019-2023 Protesilaos Stavrou <info@protesilaos.com>
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
# along with this program.  If not, see <https://www.gnu.org/licenses/>.
#
### Commentary:
#
# This script is a core part of my custom desktop session, centred
# around GNU Emacs and a tiling window manager:
# <https://git.sr.ht/~protesilaos/dotfiles>.
#
### Code:


#### Shell setup

# Load my bashrc.
if [ -n "$BASH_VERSION" ]
then
    if [ -f "$HOME"/.bashrc ]
    then
        source "$HOME"/.bashrc
    fi
fi

# Set PATH so it includes user's private executables.
if [ -d "$HOME"/bin ]
then
    PATH=$PATH:"$HOME"/bin
fi

# This is another possible location for user-specific executables.
if [ -d "$HOME"/.local/bin ]
then
    PATH=$PATH:"$HOME"/.local/bin
fi

#### Environment essentials

# TODO 2023-03-29: Is this still needed?  Maybe the systemd service is
# enough?  Or maybe I can have this here but disable the systemd part?

# Auto unlock the GPG and SSH agents.
eval $(/usr/bin/gnome-keyring-daemon --start --components=pkcs11,secrets,ssh)
export SSH_AUTH_SOCK

#### Session selection

# Launch X with the default session by running:
#
#   xinit
#
# Or specify the session:
#
#   xinit xfce
#
# Or pass this file and the DE/WM arg to startx:
#
#   startx ~/.xinitrc xfce
#
# Also check my `.bash_profile' for the automatic login setup.

#### Sessions

# Map arguments to commands.
case $session in
    bspwm ) exec bspwm        ;;
    hlwm  ) exec herbstluftwm ;;
    mate  ) exec mate-session ;;
    xfce  ) exec startxfce4   ;;
    *     ) exec herbstluftwm ;;
esac
