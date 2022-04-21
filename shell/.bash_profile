#
# ~/.bash_profile
#
# Part of my dotfiles: https://git.sr.ht/~protesilaos/dotfiles
#

# Load my configs
[ -f "$HOME"/.bashrc ] && . "$HOME"/.bashrc

# Autostart X (see my .xinitrc)
if [ -f "$HOME"/.xinitrc ]; then
    if [ -z "$DISPLAY" ] && [ "$(tty)" = /dev/tty1 ]; then
        xinit
    fi
fi
