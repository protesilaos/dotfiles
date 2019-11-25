#
# ~/.bash_profile
#
# Part of my dotfiles: https://gitlab.com/protesilaos/dotfiles
#

# Load my configs
[ -f ~/.bashrc ] && . ~/.bashrc

# Autostart X (see my .xinitrc)
if [ -f ~/.xinitrc ]; then
    if [ -z "$DISPLAY" ] && [ "$(tty)" = /dev/tty1 ]; then
        xinit
    fi
fi
