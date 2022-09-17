# ~/.bashrc --- Aliases, functions, and variables for Bash

### Description
#
# My Bash configuration file.  Any modules such as the `dircolors`
# config are stored in ~/.local/share/my_bash/.  This setup is part of
# my dotfiles.  See https://git.sr.ht/~protesilaos/dotfiles.
#
# Note that ALL MY SCRIPTS are designed to work with `#!/bin/bash`.
# They are not tested for portability.


# Shorter version of a common command that it used herein.
_checkexec ()
{
    command -v "$1" > /dev/null
}

### General settings

# Include my scripts in the PATH.
if [ -d "$HOME"/bin ]
then
    PATH=$PATH:"$HOME"/bin
fi

if [ -d "$HOME"/.local/bin ]
then
    PATH=$PATH:"$HOME"/.local/bin
fi

# May be needed for Jekyll…
if [ -d "$HOME"/.local/share/gems ]
then
    GEM_HOME="$HOME"/.local/share/gems
    PATH=$PATH:"$HOME"/.local/share/gems/bin
fi

# Needed for obs-studio
export QT_QPA_PLATFORM=wayland

# NOTE 2021-09-21: Check my systemd units.  Those are meant to work
# around the fact that Wayland does not read environment variables from
# something like ~/.profile.
export SSH_AUTH_SOCK="/run/user/$(id -u)/gnupg/S.gpg-agent.ssh"

# Default pager.  Note that the option I pass to it will quit once you
# try to scroll past the end of the file.
export PAGER="less --quit-at-eof"
export MANPAGER="$PAGER"

# Default editor.
if pgrep -x emacs > /dev/null
then
    export VISUAL="emacsclient -c"
    export EDITOR="emacsclient -t"
elif _checkexec gvim
then
    export VISUAL="gvim"
    export EDITOR=vim
else
    export VISUAL=vim
    export EDITOR=$VISUAL
fi

# Default browser.  This leverages the MIME list.
export BROWSER=/usr/bin/xdg-open

# Simple prompt
if [ -n "$SSH_CONNECTION" ]
then
    export PS1="\u@\h: \w \$ "
else
    export PS1="\w \$ "
fi
export PS2="> "

# The following is taken from the .bashrc shipped with Debian 9.  Enable
# programmable completion features (you don't need to enable this, if
# it's already enabled in /etc/bash.bashrc and /etc/profile sources
# /etc/bash.bashrc).
if ! shopt -oq posix
then
    if [ -f /usr/share/bash-completion/bash_completion ]
    then
        . /usr/share/bash-completion/bash_completion
    elif [ -f /etc/bash_completion ]
    then
        . /etc/bash_completion
    fi
fi

# Enable tab completion when starting a command with 'sudo'
[ "$PS1" ] && complete -cf sudo

# If not running interactively, don't do anything.  This too is taken
# from Debian 9's bashrc.
case $- in
    *i*) ;;
    *) return;;
esac

# Don't put duplicate lines or lines starting with space in the history.
# See `man bash` for more options.
HISTCONTROL=ignoreboth

# Append to the history file, don't overwrite it.
shopt -s histappend

# For setting history length see HISTSIZE and HISTFILESIZE in `man bash`.
HISTSIZE=1000
HISTFILESIZE=2000

# Check the window size after each command and, if necessary, update the
# values of LINES and COLUMNS.
shopt -s checkwinsize

# Make `less` more friendly for non-text input files.
_checkexec lesspipe && eval "$(SHELL=/bin/sh lesspipe)"

### Aliases

# A note on how I define aliases.  I try to abstract the command into
# its initials or something that resembles the original.  This helps me
# remember the original command when necessary.  There are some
# exceptions for commands I seldom execute.

#### Pacman and Yay (Arch Linux)

if _checkexec pacman
then
    # General package management
    alias pSyu="sudo pacman -Syu"   # system upgrade
    alias pSyyu="sudo pacman -Syyu" # when updating mirrors
    alias pD="sudo pacman -D"       # set `--asdeps` or `--asexplicit`

    # Search remote database and download packages
    alias pSs="pacman -Ss"      # search remote for package
    alias pS="sudo pacman -S"   # sync download

    # Search local database
    alias pQs="pacman -Qs"      # query list
    alias pQmq="pacman -Qmq"    # list foreign packages
    alias pQdt="pacman -Qdt"    # list orphans

    # Inspect packages (remote and local)
    alias pSi="pacman -Si"      # remote package details
    alias pQi="pacman -Qi"      # local package details

    # Remove packages
    alias pRs="sudo pacman -Rs"     # remove package
    alias pRnsc="sudo pacman -Rnsc" # remove package recursively

    # Clear cache
    alias pcache1="sudo paccache -rk 1" # remove cache except last item
    alias pcache0="sudo paccache -ruk0" # remove all cache
fi

if _checkexec yay
then
    alias ySyu="yay -Syu"       # upgrade aur
    alias yS="yay -S"           # sync download AUR
    alias ySs="yay -Ss"         # search aur
    alias ySi="yay -Si"         # see remote package details
fi

#### Common tasks and utilities

# Check these because some of them modify the behaviour of standard
# commands, such as `cp`, `mv`, `rm`, so that they provide verbose
# output and open a prompt when an existing file is affected.
#
# PRO TIP to ignore aliases, start them with a backslash \.  The
# original command will be used.  This is useful when the original
# command and the alias have the same name.  Example is my `cp` which is
# aliased to `cp -iv`:
#
#   cp == cp -iv
#   \cp == cp

# cd into the previous working directory by omitting `cd`.
alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'

# Safer default for cp, mv, rm.  These will print a verbose output of
# the operations.  If an existing file is affected, they will ask for
# confirmation.  This can make things a bit more cumbersome, but is a
# generally safer option.
alias cp='cp -iv'
alias mv='mv -iv'
alias rm='rm -Iv'

# Enable automatic color support for common commands that list output
# and also add handy aliases.  Note the link to the `dircolors`.  This
# is provided by my dotfiles.
if _checkexec dircolors
then
    dircolors_data="$HOME/.local/share/my_bash/dircolors"
    test -r $dircolors_data && eval "$(dircolors -b ${dircolors_data})" || eval "$(dircolors -b)"
fi

alias diff='diff --color=auto'
alias dir='dir --color=auto'
alias vdir='vdir --color=auto'
alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'

# Make ls a bit easier to read.  Note that the -A is the same as -a but
# does not include implied paths (the current dir denoted by a dot and
# the previous dir denoted by two dots).  I would also like to use the
# -p option, which prepends a forward slash to directories, but it does
# not seem to work with symlinked directories. For more, see `man ls`.
alias ls='ls -pv --color=auto --group-directories-first'
alias lsa='ls -pvA --color=auto --group-directories-first'
alias lsl='ls -lhpv --color=auto --group-directories-first'
alias lsla='ls -lhpvA --color=auto --group-directories-first'

#### Extra tasks and infrequently used tools

# Quick shortcuts for `mpv`.  When I want to play a podcast that only
# shows a static image, I run the command with the --no-video option.
# When I only need to view the file I use --no-audio.  The one with
# --ytdl-raw-options is for those occasions where a video is 4k or
# something that slows things down considerably.
if _checkexec mpv
then
    alias mpvna='mpv --no-audio'
    alias mpvnv='mpv --no-video'
    alias mpvhd="mpv --ytdl-raw-options='format=[[bestvideo=height<=720]]'"
fi

# Quick shortcuts for `yt-dlp`.  Output is placed in the present working
# directory.
if _checkexec yt-dlp
then
    alias ytaud='yt-dlp --add-metadata -ci --extract-audio --audio-format mp3 -o "%(title)s.%(ext)s"'
    alias ytvid='yt-dlp --add-metadata --no-playlist --no-part --write-description --newline --prefer-free-formats -o "%(title)s.%(ext)s" '
fi

# Just for fun.
alias 👽='sudo'
alias 📦='pacman'
alias 👺='vim'
alias 🦄='emacsclient -c -a vim'
alias 🐵='cp -iv'
alias 🤡='whoami'

if _checkexec notmuch
then
    # Careful with 'tag:del'!!!  It is specific to my setup.
    alias ✉️💀='notmuch search --output=files --format=text0 tag:del | xargs -r0 rm'
    alias notmuchrmdel='notmuch search --output=files --format=text0 tag:del | xargs -r0 rm'
fi

### Functions

# Colourise man pages
man ()
{
    env \
        LESS_TERMCAP_mb=$(tput bold; tput setaf 6) \
        LESS_TERMCAP_md=$(tput bold; tput setaf 6) \
        LESS_TERMCAP_me=$(tput sgr0) \
        LESS_TERMCAP_se=$(tput rmso; tput sgr0) \
        LESS_TERMCAP_ue=$(tput rmul; tput sgr0) \
        LESS_TERMCAP_us=$(tput smul; tput bold; tput setaf 4) \
        LESS_TERMCAP_mr=$(tput rev) \
        LESS_TERMCAP_mh=$(tput dim) \
        LESS_TERMCAP_ZN=$(tput ssubm) \
        LESS_TERMCAP_ZV=$(tput rsubm) \
        LESS_TERMCAP_ZO=$(tput ssupm) \
        LESS_TERMCAP_ZW=$(tput rsupm) \
        man "$@"
}

# Enter directory and list contents
cd ()
{
    if [ -n "$1" ]
    then
        builtin cd "$@" && ls -pvA --color=auto --group-directories-first
    else
        builtin cd ~ && ls -pvA --color=auto --group-directories-first
    fi
}

# Back up a file. Usage "backupthis <filename>"
backupthis ()
{
    cp -riv $1 ${1}-$(date +%Y%m%d%H%M).backup;
}
