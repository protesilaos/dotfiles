#!/bin/bash

# xtwm_common --- Common settings for Xorg Tiling Window Managers
#
# Copyright (c) 2023  Protesilaos Stavrou <info@protesilaos.com>
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
# Common settings used by my BSPWM and Herbstluftwm setups
# (collectively referred to as "Xorg Tiling Window Managers").  I
# configure those window managers to be similar.
#
# In all cases where a file is called, I assume it exists.
#
# Part of my dotfiles: <https://github.com/protesilaos/dotfiles>.

### Code:

_check ()
{
    command -v "$1" > /dev/null
}

# Specify keyboard layout options.  See the setxkbmap(1) manual for
# the command line arguments and the xkeyboard-config(7) manual for
# the layout names and options.
#
# NOTE 2023-07-16: The -option used to accept a comma separated list.
# Now it requires each option to be specified separately.
#
# NOTE 2023-07-16: The Greek layout used to be called 'el' but now is
# 'gr'.
if _check setxkbmap
then
    # setxkbmap -layout 'us,gr' -option '' -option 'ctrl:nocaps' \
    #           -option 'altwin:menu_win' -option 'caps:none' \
    #           -option 'compose:ins' -option 'grp:win_space_toggle'
    setxkbmap -layout 'us,gr' -option '' -option 'compose:ins'
fi

# The hotkey daemon that handles all custom key bindings.  I split the
# key chords into separate files, based on their scope.  The ones that
# pertain to the window manager are defined in sxhkdrc_bspwm and
# sxhkdrc_herbstluftwm, respectively.
_check sxhkd && sxhkd -c "$HOME"/.config/sxhkd/sxhkdrc{,_"$DESKTOP_SESSION"} &

# Launch Polybar
if _check polybar
then
    # Make sure we have no running instances
    pkill -x polybar

    # I name my bars after the bspwm or herbstluftwm sessions, so I
    # launch the one I need by getting the relevant environment
    # variable.
    polybar -rq "$DESKTOP_SESSION" &
fi

# Run the tool that sets the wallpaper.  A program could be used that
# handles the desktop in general, including the option to set icons.
# But this is enough for my case.  Here is a command I tested and which
# works (using the Xfce desktop manager):
# xfdesktop --sm-client-disable --disable-wm-check &
if _check feh
then
    if [ -f "$HOME"/.fehbg ]
    then
        "$HOME"/.fehbg &
    fi
fi

## NOTE 2023-01-19: I added the `--backend glx`.  Maybe this is
## better?  I will see it in action when I get the chance.
#
# FIXME 2021-12-05: It create artifacts while streaming/recording
# video whenever I scroll in Emacs.

# Start the display compositor.  See `picom --help'.
_check picom && picom --config /dev/null -r 3 -o 0.55 -l -2 -t -2 \
                      -I 0.075 -O 0.075 -D 8 -f -i 1.0 -e 1.0 \
                      --detect-transient --detect-client-leader --vsync \
                      --backend glx --glx-no-stencil --glx-no-rebind-pixmap &

# Simple daemon for GTK settings
_check xsettingsd && xsettingsd -c "$HOME"/.config/xsettingsd/xsettingsd.conf &

# Use the same cursor everywhere
_check xsetroot && xsetroot -xcf /usr/share/icons/Adwaita/cursors/left_ptr 16

# Load my Xresources
_check xrdb && [ -f "$HOME"/.Xresources ] && xrdb -I "$HOME" -merge "$HOME"/.Xresources
