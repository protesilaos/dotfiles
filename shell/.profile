# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022

# if running bash
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
	. "$HOME/.bashrc"
    fi
fi

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/bin" ]; then
    PATH=$PATH:"$HOME"/bin
fi

if [ -d "$HOME/.local/bin" ]; then
    PATH=$PATH:"$HOME"/.local/bin
fi

# unclock keyring for terminal sessions
# see https://wiki.archlinux.org/index.php/GNOME/Keyring#With_a_display_manager
if [ -n "$DESKTOP_SESSION" ]; then
    eval $(gnome-keyring-daemon --start --daemonize)
    export SSH_AUTH_SOCK
fi
