#!/bin/bash

# Delight (DE light) --- Toggle DE/WM and Emacs themes
#
# Copyright (c) 2020-2025  Protesilaos Stavrou <info@protesilaos.com>
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
# Simple tool to switch between light and dark themes.  Primarily
# centred around my BSPWM/HerbstluftWM or Sway setup.  For the standard
# DEs, this has been tested with GNOME 42 and Xfce 4.16.
#
# Part of my dotfiles: <https://github.com/protesilaos/dotfiles>.

### Code:

#### General

gtk_theme_light="Adwaita"
gtk_theme_dark="Adwaita-dark"
icon_theme_light="Papirus-Light"
icon_theme_dark="Papirus-Dark"
# icon_theme_light="Adwaita"
# icon_theme_dark="Adwaita"

_depcheck ()
{
    command -v "$1" > /dev/null || { echo "Missing dependency: $1."; return 1; }
}

_sed ()
{
    # NOTE 2021-08-27: Confirm the following.
    #
    # If we run this asynchronously other processes might fail to get
    # the new colours.  To my knowledge as of 2019-06-27, this appears
    # to be better than using sleep and/or until...
    sed --follow-symlinks -i "$@"
}

_emacs ()
{
    _depcheck emacs

    pgrep -x emacs > /dev/null || return 1

    # Just switch to the appropriate theme for Emacs:
    # https://github.com/protesilaos/modus-themes
    case "$1"
    in
        # TODO 2023-03-01: Define a function to load appropriate Emacs
        # theme.
        l*) emacsclient -e "(when (and (functionp 'modus-themes-get-themes) (modus-themes-get-themes)) (modus-themes-load-random-light))" > /dev/null ;;
        d*) emacsclient -e "(when (and (functionp 'modus-themes-get-themes) (modus-themes-get-themes)) (modus-themes-load-random-dark))"  > /dev/null ;;
    esac
}

_xterm ()
{
    _depcheck xterm

    xresources="$HOME"/.Xresources
    active_theme="$HOME"/.config/xterm/xterm_active_theme

    [ -f "$xresources" ] || { echo "No Xresources file"; exit 1; }
    [ -f "$active_theme" ] || { echo "No active Xresources theme"; exit 1; }

    case $wm_theme
    in
        light) _sed "s,vivendi,operandi," "$active_theme" ;;
        dark)  _sed "s,operandi,vivendi," "$active_theme" ;;
    esac

    xrdb -I "$HOME" -merge "$xresources"

    # Another one of my scripts for live recolouring running terminal
    # emulators, by using escape sequences.
    repaint_terminals.sh &
}

_xfce4terminal ()
{
    _depcheck xfce4-terminal

    active_theme="$HOME"/.config/xfce4/terminal/terminalrc
    palette_light="#000000;#a60000;#006800;#6f5500;#0031a9;#8f0075;#005e8b;#aaaaaa;#505050;#972500;#00663f;#884900;#0000b0;#531ab6;#005f5f;#ffffff;"
    palette_dark="#000000;#ff5f59;#44bc44;#d0bc00;#2fafff;#f78fe7;#00d3d0;#aaaaaa;#505050;#ff6b55;#00c06f;#fec43f;#00bcff;#b6a0ff;#6ae4b9;#ffffff;"

    case "$1"
    in
        light)
            _sed "s,ColorForeground=.*,ColorForeground=#000000," "$active_theme"
            _sed "s,ColorBackground=.*,ColorBackground=#ffffff," "$active_theme"
            _sed "s,ColorPalette=.*,ColorPalette=$palette_light," "$active_theme"
            ;;
        dark)
            _sed "s,ColorForeground=.*,ColorForeground=#ffffff," "$active_theme"
            _sed "s,ColorBackground=.*,ColorBackground=#000000," "$active_theme"
            _sed "s,ColorPalette=.*,ColorPalette=$palette_dark," "$active_theme"
            ;;
    esac
}

# TODO 2021-08-27: dunst

#### BSPWM and HerbstluftWM (I configure them almost identically)

_rofi ()
{
    _depcheck rofi

    active_theme="$HOME"/.config/rofi/themes/modus-themes-active.rasi

    [ -f "$active_theme" ] || { echo "No rofi theme. Did you 'stow' your configurations?"; return 1; }

    case $wm_theme
    in
        light) echo -e '@import "modus-operandi.rasi"' > "$active_theme" ;;
        dark)  echo -e '@import "modus-vivendi.rasi"' > "$active_theme"  ;;
    esac
}

_polybar ()
{
    _depcheck polybar

    pgrep -x polybar > /dev/null && pkill -x polybar

    active_theme="$HOME"/.config/polybar/config_colors

    [ -f "$active_theme" ] || { echo "No active theme"; exit 1; }

    case $wm_theme
    in
        light) _sed "s,vivendi,operandi," "$active_theme" ;;
        dark)  _sed "s,operandi,vivendi," "$active_theme" ;;
    esac

    [ -n "$1" ] && polybar -rq "$DESKTOP_SESSION" &
}

_feh ()
{
    _depcheck feh

    image_light="$HOME"/Pictures/light.jpg
    image_dark="$HOME"/Pictures/dark.jpg

    case $wm_theme
    in
        light) [ -f "$image_light" ] && feh --bg-fill "$image_light" ;;
        dark)  [ -f "$image_dark" ]  && feh --bg-fill "$image_dark"  ;;
    esac
}

_xsettingsd ()
{
    _gtk_common ()
    {
        # # GTK 2
        # _sed "s,\(gtk-theme-name=\).*,\1\'${1}\', ; s,\(gtk-icon-theme-name=\).*,\1\'${2}\'," \
            #      "$HOME"/.gtkrc-2.0

        # GTK 3
        _sed "s,\(gtk-theme-name=\).*,\1${1}, ; s,\(gtk-icon-theme-name=\).*,\1${2}," \
             "$HOME"/.config/gtk-3.0/settings.ini
    }

    _gtk_live ()
    {
        _depcheck xsettingsd

        xsettingsd="$HOME"/.config/xsettingsd/xsettingsd.conf

        if [ -f "$xsettingsd" ]
        then
            _sed "s,\(.*\/ThemeName\) \"[0-9a-zA-Z-]*\",\1 \"${1}\"," "$xsettingsd"
            _sed "s,\(.*\/IconThemeName\) \"[0-9a-zA-Z-]*\",\1 \"${2}\"," "$xsettingsd"
        fi

        # This is a very lightweight program that simply loads the
        # settings it reads from the file.  Kill and run again to read
        # the new values.
        pgrep -xo xsettingsd > /dev/null && pkill -xo xsettingsd
        xsettingsd -c "$xsettingsd" &
    }

    case "$1"
    in
        light)
            _gtk_common "$gtk_theme_light" "$icon_theme_light"
            _gtk_live "$gtk_theme_light" "$icon_theme_light"
            ;;
        dark)
            _gtk_common "$gtk_theme_dark" "$icon_theme_dark"
            _gtk_live "$gtk_theme_dark" "$icon_theme_dark"
            ;;
    esac
}

_bspwm_or_hlwm ()
{
    _depcheck "$wm_executable"

    _gnome_theme

    wm_theme_file="$HOME"/.config/prot-xtwm-active-theme
    focus_mode_status="$HOME"/.config/prot-xtwm-focus-mode

	echo "$style" > "$wm_theme_file"
    wm_theme="$style"

    _depcheck "$conf_colors" && "$conf_colors"
    _emacs "$style" &
    _xsettingsd "$style" &
    _xterm &
    _xfce4terminal "$style" &
    # # NOTE 2021-09-25: I tried alacritty for a while, but the way it
    # reads the fonts on Xorg makes the point size larger than expected.
    # Also, it seems to miss some smaller sizes, so I cannot use it
    # exactly how I want to.  Everything is fine on Wayland with Sway...
    #
    # _alacritty &
    _feh &
    _rofi &

    if [ -f "$focus_mode_status" ] && [ "$(sed 1q "$focus_mode_status")" = off ]
    then
        _polybar launch
    else
        # Just update the colours, but do not launch the process.
        _polybar
    fi
}

# TODO 2023-10-05: Merge with `_bspwm_or_hlwm'?
_i3 ()
{
    _gnome_theme

    wm_theme_file="$HOME"/.config/prot-xtwm-active-theme
    # focus_mode_status="$HOME"/.config/prot-xtwm-focus-mode

	echo "$style" > "$wm_theme_file"
    wm_theme="$style"

    _emacs "$style" &
    _xsettingsd "$style" &
    _xterm &
    # # NOTE 2021-09-25: I tried alacritty for a while, but the way it
    # reads the fonts on Xorg makes the point size larger than expected.
    # Also, it seems to miss some smaller sizes, so I cannot use it
    # exactly how I want to.  Everything is fine on Wayland with Sway...
    #
    # _alacritty &
    _feh &
    _rofi &

    #  TODO 2023-10-05: focus mode for i3

    # if [ -f $focus_mode_status ] && [ "$(sed 1q "$focus_mode_status")" = off ]
    # then
    #     _polybar launch
    # else
    #     # Just update the colours, but do not launch the process.
    #     _polybar
    # fi

    _polybar launch

    active_theme="$HOME"/.config/i3/i3-theme

    [ -f "$active_theme" ] || { echo "No active theme"; exit 1; }

    case $wm_theme
    in
        light) _sed "s,vivendi,operandi," "$active_theme" ;;
        dark)  _sed "s,operandi,vivendi," "$active_theme" ;;
    esac

    i3-msg -t command reload
}

#### Gsettings

# Get or set the GTK theme
_gsettings_gtk_act_theme_interface ()
{
    gsettings "$1" org.gnome.desktop.interface gtk-theme "${@:2}"
}

# Set the Color Scheme
_gsettings_color_scheme_set ()
{
    gsettings set org.gnome.desktop.interface color-scheme "$1"
}

##### Gnome

_gnome_theme ()
{
    _depcheck gsettings

    if [ "$(_gsettings_gtk_act_theme_interface get)" = "'$gtk_theme_light'" ]
    then
        _gsettings_gtk_act_theme_interface set "$gtk_theme_dark"
    else
        _gsettings_gtk_act_theme_interface set "$gtk_theme_light"
    fi

    if [ "$(gsettings get org.gnome.desktop.interface color-scheme)" = "'prefer-light'" ]
    then
        style=dark
        _gsettings_color_scheme_set prefer-dark
    else
        style=light
        _gsettings_color_scheme_set prefer-light
    fi
}

_gnome ()
{
    _gnome_theme
    _emacs "$style"
}

#### Xfce

_xfquery_get ()
{
    xfconf-query -c "$1" -p "$2"
}

_xfquery_set ()
{
    xfconf-query -c "$1" -p "$2" -s "$3"
}

_xfce ()
{
    _depcheck gsettings

    if [ "$(_xfquery_get 'xsettings' '/Net/ThemeName')" = "$gtk_theme_light" ]
    then
        style=dark
        # _xfquery_set 'xfwm4' '/general/theme' 'adwaita-dark'
        _gsettings_gtk_act_theme_interface set "$gtk_theme_dark"
        _xfquery_set 'xsettings' '/Net/ThemeName' "$gtk_theme_dark"
        _xfquery_set 'xsettings' '/Net/IconThemeName' "$icon_theme_dark"
        _gsettings_color_scheme_set 'prefer-dark'
    else
        style=light
        # _xfquery_set 'xfwm4' '/general/theme' 'adwaita'
        _gsettings_gtk_act_theme_interface set "$gtk_theme_light"
        _xfquery_set 'xsettings' '/Net/ThemeName' "$gtk_theme_light"
        _xfquery_set 'xsettings' '/Net/IconThemeName' "$icon_theme_light"
        _gsettings_color_scheme_set 'prefer-light'
    fi

    _emacs "$style" &
    _xfce4terminal "$style" &
}

#### Sessions

case "$DESKTOP_SESSION"
in
    bspwm)
        wm_executable=bspwm
        conf_colors=bspwm_conf_colors.sh

        _bspwm_or_hlwm
        ;;
    herbstluftwm)
        wm_executable=herbstluftwm
        conf_colors=herbstluftwm_conf_colors.sh

        _bspwm_or_hlwm
        ;;
    i3) _i3 ;;
    gnome) _gnome ;;
    xfce) _xfce ;;

esac
