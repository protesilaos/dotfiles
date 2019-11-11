# auto-unlocks the gpg and ssh agents
eval $(gnome-keyring-daemon --start)
export SSH_AUTH_SOCK

if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
        . "$HOME/.bashrc"
    fi
fi

# set PATH so it includes user's private executables
if [ -d "$HOME/bin" ]; then
    PATH=$PATH:"$HOME"/bin
fi

if [ -d "$HOME/.local/bin" ]; then
    PATH=$PATH:"$HOME"/.local/bin
fi

# Launch X by running:
# 	xinit
#   xinit xfce
#
# Or pass this file and the DE/WM arg to startx:
#   startx ~/.xinitrc bspwm

# This is the default
session=${1:-bspwm}

# Map arguments to commands.
case $session in
    bspwm ) exec bspwm        ;;
    mate  ) exec mate-session ;;
    xfce  ) exec startxfce4   ;;
    *) echo "Unknown session" ;;
esac
