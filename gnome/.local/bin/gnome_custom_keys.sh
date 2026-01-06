#!/bin/bash

# gnome_custom_keys.sh --- My custom key bindings for GNOME
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
# My custom key bindings for GNOME.  This may not be complete but it
# fixes all the known keybinding-related conflicts I encounter
# whenever I want to use Emacs inside of GNOME.
#
# Part of my dotfiles: <https://github.com/protesilaos/dotfiles>.

### Code:

gsettings set org.gnome.TextEditor keybindings 'default'
gsettings set org.gnome.desktop.wm.keybindings activate-window-menu "['']"
gsettings set org.gnome.desktop.wm.keybindings always-on-top "['']"
gsettings set org.gnome.desktop.wm.keybindings begin-move "['']"
gsettings set org.gnome.desktop.wm.keybindings begin-resize "['']"
gsettings set org.gnome.desktop.wm.keybindings close "['<Super>q']"
gsettings set org.gnome.desktop.wm.keybindings cycle-group "['']"
gsettings set org.gnome.desktop.wm.keybindings cycle-group-backward "['']"
gsettings set org.gnome.desktop.wm.keybindings cycle-panels "['']"
gsettings set org.gnome.desktop.wm.keybindings cycle-panels-backward "['']"
gsettings set org.gnome.desktop.wm.keybindings cycle-windows "['']"
gsettings set org.gnome.desktop.wm.keybindings cycle-windows-backward "['']"
gsettings set org.gnome.desktop.wm.keybindings lower "['']"
gsettings set org.gnome.desktop.wm.keybindings maximize "['']"
gsettings set org.gnome.desktop.wm.keybindings maximize-horizontally "['']"
gsettings set org.gnome.desktop.wm.keybindings maximize-vertically "['']"
gsettings set org.gnome.desktop.wm.keybindings minimize "['<Super>h']"
gsettings set org.gnome.desktop.wm.keybindings move-to-center "['']"
gsettings set org.gnome.desktop.wm.keybindings move-to-corner-ne "['']"
gsettings set org.gnome.desktop.wm.keybindings move-to-corner-nw "['']"
gsettings set org.gnome.desktop.wm.keybindings move-to-corner-se "['']"
gsettings set org.gnome.desktop.wm.keybindings move-to-corner-sw "['']"
gsettings set org.gnome.desktop.wm.keybindings move-to-monitor-down "['']"
gsettings set org.gnome.desktop.wm.keybindings move-to-monitor-left "['']"
gsettings set org.gnome.desktop.wm.keybindings move-to-monitor-right "['']"
gsettings set org.gnome.desktop.wm.keybindings move-to-monitor-up "['']"
gsettings set org.gnome.desktop.wm.keybindings move-to-side-e "['']"
gsettings set org.gnome.desktop.wm.keybindings move-to-side-n "['']"
gsettings set org.gnome.desktop.wm.keybindings move-to-side-s "['']"
gsettings set org.gnome.desktop.wm.keybindings move-to-side-w "['']"
gsettings set org.gnome.desktop.wm.keybindings move-to-workspace-1 "['']"
gsettings set org.gnome.desktop.wm.keybindings move-to-workspace-10 "['']"
gsettings set org.gnome.desktop.wm.keybindings move-to-workspace-11 "['']"
gsettings set org.gnome.desktop.wm.keybindings move-to-workspace-12 "['']"
gsettings set org.gnome.desktop.wm.keybindings move-to-workspace-2 "['']"
gsettings set org.gnome.desktop.wm.keybindings move-to-workspace-3 "['']"
gsettings set org.gnome.desktop.wm.keybindings move-to-workspace-4 "['']"
gsettings set org.gnome.desktop.wm.keybindings move-to-workspace-5 "['']"
gsettings set org.gnome.desktop.wm.keybindings move-to-workspace-6 "['']"
gsettings set org.gnome.desktop.wm.keybindings move-to-workspace-7 "['']"
gsettings set org.gnome.desktop.wm.keybindings move-to-workspace-8 "['']"
gsettings set org.gnome.desktop.wm.keybindings move-to-workspace-9 "['']"
gsettings set org.gnome.desktop.wm.keybindings move-to-workspace-down "['']"
gsettings set org.gnome.desktop.wm.keybindings move-to-workspace-last "['']"
gsettings set org.gnome.desktop.wm.keybindings move-to-workspace-left "['<Shift><Super>Left']"
gsettings set org.gnome.desktop.wm.keybindings move-to-workspace-right "['<Shift><Super>Right']"
gsettings set org.gnome.desktop.wm.keybindings move-to-workspace-up "['']"
gsettings set org.gnome.desktop.wm.keybindings panel-main-menu "['']"
gsettings set org.gnome.desktop.wm.keybindings panel-run-dialog "['']"
gsettings set org.gnome.desktop.wm.keybindings raise "['']"
gsettings set org.gnome.desktop.wm.keybindings raise-or-lower "['']"
gsettings set org.gnome.desktop.wm.keybindings set-spew-mark "['']"
gsettings set org.gnome.desktop.wm.keybindings show-desktop "['']"
gsettings set org.gnome.desktop.wm.keybindings switch-applications "['<Super>Tab']"
gsettings set org.gnome.desktop.wm.keybindings switch-applications-backward "['<Shift><Super>Tab']"
gsettings set org.gnome.desktop.wm.keybindings switch-group "['<Super>grave']"
gsettings set org.gnome.desktop.wm.keybindings switch-group-backward "['<Shift><Super>grave']"
gsettings set org.gnome.desktop.wm.keybindings switch-input-source "['<Super>space', 'XF86Keyboard']"
gsettings set org.gnome.desktop.wm.keybindings switch-input-source-backward "['<Shift><Super>space', '<Shift>XF86Keyboard']"
gsettings set org.gnome.desktop.wm.keybindings switch-panels "['']"
gsettings set org.gnome.desktop.wm.keybindings switch-panels-backward "['']"
gsettings set org.gnome.desktop.wm.keybindings switch-to-workspace-1 "['']"
gsettings set org.gnome.desktop.wm.keybindings switch-to-workspace-10 "['']"
gsettings set org.gnome.desktop.wm.keybindings switch-to-workspace-11 "['']"
gsettings set org.gnome.desktop.wm.keybindings switch-to-workspace-12 "['']"
gsettings set org.gnome.desktop.wm.keybindings switch-to-workspace-2 "['']"
gsettings set org.gnome.desktop.wm.keybindings switch-to-workspace-3 "['']"
gsettings set org.gnome.desktop.wm.keybindings switch-to-workspace-4 "['']"
gsettings set org.gnome.desktop.wm.keybindings switch-to-workspace-5 "['']"
gsettings set org.gnome.desktop.wm.keybindings switch-to-workspace-6 "['']"
gsettings set org.gnome.desktop.wm.keybindings switch-to-workspace-7 "['']"
gsettings set org.gnome.desktop.wm.keybindings switch-to-workspace-8 "['']"
gsettings set org.gnome.desktop.wm.keybindings switch-to-workspace-9 "['']"
gsettings set org.gnome.desktop.wm.keybindings switch-to-workspace-down "['']"
gsettings set org.gnome.desktop.wm.keybindings switch-to-workspace-last "['']"
gsettings set org.gnome.desktop.wm.keybindings switch-to-workspace-left "['']"
gsettings set org.gnome.desktop.wm.keybindings switch-to-workspace-right "['']"
gsettings set org.gnome.desktop.wm.keybindings switch-to-workspace-up "['']"
gsettings set org.gnome.desktop.wm.keybindings switch-windows "['<Alt>Tab']"
gsettings set org.gnome.desktop.wm.keybindings switch-windows-backward "['<Shift><Alt>Tab']"
gsettings set org.gnome.desktop.wm.keybindings toggle-above "['']"
gsettings set org.gnome.desktop.wm.keybindings toggle-fullscreen "['<Super>f']"
gsettings set org.gnome.desktop.wm.keybindings toggle-maximized "['<Super>m']"
gsettings set org.gnome.desktop.wm.keybindings toggle-on-all-workspaces "['']"
gsettings set org.gnome.desktop.wm.keybindings toggle-shaded "['']"
gsettings set org.gnome.desktop.wm.keybindings unmaximize "['']"
gsettings set org.gnome.metacity.keybindings toggle-tiled-left "['<Super>Left']"
gsettings set org.gnome.metacity.keybindings toggle-tiled-right "['<Super>Right']"
gsettings set org.gnome.mutter.keybindings rotate-monitor "['']"
gsettings set org.gnome.mutter.keybindings switch-monitor "['']"
gsettings set org.gnome.mutter.keybindings tab-popup-cancel "['']"
gsettings set org.gnome.mutter.keybindings tab-popup-select "['']"
gsettings set org.gnome.mutter.keybindings toggle-tiled-left "['<Super>Left']"
gsettings set org.gnome.mutter.keybindings toggle-tiled-right "['<Super>Right']"
gsettings set org.gnome.mutter.wayland.keybindings restore-shortcuts "['']"
gsettings set org.gnome.mutter.wayland.keybindings switch-to-session-1 "['']"
gsettings set org.gnome.mutter.wayland.keybindings switch-to-session-10 "['']"
gsettings set org.gnome.mutter.wayland.keybindings switch-to-session-11 "['']"
gsettings set org.gnome.mutter.wayland.keybindings switch-to-session-12 "['']"
gsettings set org.gnome.mutter.wayland.keybindings switch-to-session-2 "['']"
gsettings set org.gnome.mutter.wayland.keybindings switch-to-session-3 "['']"
gsettings set org.gnome.mutter.wayland.keybindings switch-to-session-4 "['']"
gsettings set org.gnome.mutter.wayland.keybindings switch-to-session-5 "['']"
gsettings set org.gnome.mutter.wayland.keybindings switch-to-session-6 "['']"
gsettings set org.gnome.mutter.wayland.keybindings switch-to-session-7 "['']"
gsettings set org.gnome.mutter.wayland.keybindings switch-to-session-8 "['']"
gsettings set org.gnome.mutter.wayland.keybindings switch-to-session-9 "['']"
gsettings set org.gnome.settings-daemon.plugins.media-keys custom-keybindings "['/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom0/', '/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom1/']"
gsettings set org.gnome.shell.keybindings focus-active-notification "['']"
gsettings set org.gnome.shell.keybindings open-application-menu "['']"
gsettings set org.gnome.shell.keybindings screenshot "['<Shift>Print']"
gsettings set org.gnome.shell.keybindings screenshot-window "['<Alt>Print']"
gsettings set org.gnome.shell.keybindings shift-overview-down "['']"
gsettings set org.gnome.shell.keybindings shift-overview-up "['']"
gsettings set org.gnome.shell.keybindings show-screen-recording-ui "['']"
gsettings set org.gnome.shell.keybindings show-screenshot-ui "['Print']"
gsettings set org.gnome.shell.keybindings switch-to-application-1 "['']"
gsettings set org.gnome.shell.keybindings switch-to-application-2 "['']"
gsettings set org.gnome.shell.keybindings switch-to-application-3 "['']"
gsettings set org.gnome.shell.keybindings switch-to-application-4 "['']"
gsettings set org.gnome.shell.keybindings switch-to-application-5 "['']"
gsettings set org.gnome.shell.keybindings switch-to-application-6 "['']"
gsettings set org.gnome.shell.keybindings switch-to-application-7 "['']"
gsettings set org.gnome.shell.keybindings switch-to-application-8 "['']"
gsettings set org.gnome.shell.keybindings switch-to-application-9 "['']"
gsettings set org.gnome.shell.keybindings toggle-application-view "['<Super>a']"
gsettings set org.gnome.shell.keybindings toggle-message-tray "['']"
gsettings set org.gnome.shell.keybindings toggle-overview "['<Super>d']"
gsettings set org.gtk.Settings.Debug enable-inspector-keybinding false
gsettings set org.gtk.gtk4.Settings.Debug enable-inspector-keybinding true
