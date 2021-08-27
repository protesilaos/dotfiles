# .profile --- Initialisation script for display managers.
#
# Copyright (c) 2020 Protesilaos Stavrou <info@protesilaos.com>
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
# This script is read by a graphical display manager when logging into
# the session.  It is conceptually similar to `.xinitrc' and I am
# considering it as practically equivalent to it for the ordinary
# functioning of my custom desktop session.  Refer to my dotfiles for
# the specifics: https://gitlab.com/protesilaos/dotfiles


###############
# Shell setup #
###############

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

##########################
# Environment essentials #
##########################

# Auto unlocks the GPG and SSH agents.
eval $(/usr/bin/gnome-keyring-daemon --start --components=pkcs11,secrets,ssh)
export SSH_AUTH_SOCK

# Load my XTerm and relevant configurations.
[ -f "$HOME"/.Xresources ] && xrdb -I "$HOME" -merge "$HOME"/.Xresources
