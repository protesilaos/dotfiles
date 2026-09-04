#!/bin/bash

# herbstluftwm_conf_colors --- Configure color settings for HerbstluftWM.
#
# Copyright (C) 2021-2026  Protesilaos <info@protesilaos.com>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.	If not, see <https://www.gnu.org/licenses/>.

_hc ()
{
    herbstclient "$@"
}

# Values come from my Modus themes for GNU Emacs:
# <https://protesilaos.com/modus-themes>.
if [ -f "$HOME"/.config/prot-xtwm-active-theme ]
then
   case "$(cat "$HOME"/.config/prot-xtwm-active-theme)" in
       dark*)
           bg_main="#000000"
           bg_dim="#1e1e1e"
           fg_main="#ffffff"
           fg_dim="#989898"
           border_urgent="#ff8059"
           ;;
       *)
           bg_main="#ffffff"
           bg_dim="#f0f0f0"
           fg_main="#000000"
           fg_dim="#595959"
           border_urgent="#a60000"
           ;;
   esac

   _hc set frame_border_active_color "$bg_main"
   _hc set frame_bg_active_color "$fg_dim"
   _hc attr theme.active.color "$bg_main"
   _hc attr theme.active.inner_color "$fg_dim"
   _hc attr theme.active.title_color "$fg_main"

   _hc set frame_border_normal_color "$bg_dim"
   _hc set frame_bg_normal_color "$bg_dim"
   _hc attr theme.normal.color "$bg_dim"
   _hc attr theme.normal.inner_color "$bg_dim"
   _hc attr theme.normal.title_color "$fg_dim"

   _hc attr theme.urgent.color "$border_urgent"
   _hc attr theme.urgent.inner_color "$border_urgent"
   _hc attr theme.urgent.title_color "$bg_main"

   # The following are counter-intuitive names for inactive tabs of
   # the frame.  The active tab gets the border colour.  So
   # theme.active.tab_color actually means "active tab of inactive
   # frame"...  Quote from herbstluftwm(1) with ALL CAPS mine:
   #
   # Setting an attribute of the theme object just propagates the
   # value to the respective attribute of the tiling and the floating
   # object. IF THE TITLE AREA IS DIVIDED INTO TABS, THEN THE NOT
   # SELECTED TABS CAN BE STYLED USING THE TAB_... ATTRIBUTES. If
   # these attributes are empty, then the colors are taken from the
   # theme of the client to which the tab refers to.
   _hc attr theme.active.tab_color "$bg_dim"
   _hc attr theme.normal.tab_color "$bg_dim"
   _hc attr theme.urgent.tab_color "$border_urgent"

   _hc attr theme.floating.outer_color "$bg_main"

   # This is used when resizing and the window contents do not stretch
   # immediately to fill in the space.
   _hc attr theme.background_color "$bg_dim"

   for state in active urgent normal
   do
    _hc substitute C theme.${state}.inner_color \
        attr theme.${state}.outer_color C
   done
fi
