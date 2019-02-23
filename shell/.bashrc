#
# ~/.bashrc
#

# Description {{{
# ===============
#
# BASH configuration file.  Any modules such as the `dircolors` config
# are stored in ~/.local/share/my_bash/.  This setup is part of my
# dotfiles.  See https://gitlab.com/protesilaos/dotfiles.
#
# Note that ALL MY SCRIPTS are designed to work with `#!/bin/bash`.
# They are not tested for portability.
#
# Last reviewed on 2018-11-18

# }}}

# General settings {{{
# ====================

# Include my scripts in the PATH.  To avoid conflicts, I always prepend
# `own_script_` to my files.  There are some exceptions though, where I
# am confident that no conflicts will arrise.  See the 'bin' directory
# of my dotfiles.
if [ -d "$HOME/bin" ] ; then
    export PATH=$PATH:"$HOME/bin"
fi

# Default pager.  Note that the option I pass to it will quit once you
# try to scroll past the end of the file.
export PAGER="less --quit-at-eof"
export MANPAGER=$PAGER

# Default editor.  On Debian the Vim GUI is provided by a separate
# package.
if [ -x /usr/bin/gvim ]; then
    export VISUAL="gvim"
    export EDITOR=vim
else
    export VISUAL=vim
    export EDITOR=$VISUAL
fi

# Default browser.  This leverages the MIME list.
export BROWSER=/usr/bin/xdg-open

# Simple prompt
if [ -n "$SSH_CONNECTION" ]; then
    export PS1="\u@\h: \w \$ "
else
    export PS1="\w \$ "
fi
export PS2="> "

# The following is taken from the .bashrc shipped with Debian 9.  Enable
# programmable completion features (you don't need to enable this, if
# it's already enabled in /etc/bash.bashrc and /etc/profile sources
# /etc/bash.bashrc).
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi

# Enable tab completion when starting a command with 'sudo'
if [ "$PS1" ]; then
    complete -cf sudo
fi

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

# Make less more friendly for non-text input files, see `man lesspipe`.
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# cd into a directory by typing its name.  I disable this but keep it
# around.  I find it tricky, especially when an executable has a
# similar/same name to a directory.
# shopt -s autocd

# }}}

# Aliases {{{
# ===========
# A note on how I define aliases.  I try to abstract the command into
# its initials or something that resembles the original.  This helps me
# remember the original command when necessary.  There are some
# exceptions for commands I seldom execute.

# APT (package management on Debian) {{{
# --------------------------------------

# If you are coming to Debian from Arch-based distros, check
# compatibility with `pacman`:
# https://wiki.archlinux.org/index.php/Pacman/Rosetta
if [ -x /usr/bin/apt ]; then
	# up{dating,grading}.  The -V shows version changes.
	alias au="sudo apt update"
	alias aug="sudo apt upgrade -V"
	alias auu="sudo apt update && sudo apt upgrade -V"
	alias afu="sudo apt full-upgrade"
	alias auufu="sudo apt update && sudo apt upgrade -V && sudo apt full-upgrade"

	# act on package targets
	alias ai="sudo apt install"
	alias air="sudo apt install --reinstall"
	alias ar="sudo apt remove"

	# list local packages
	alias ali="apt list --installed"
	alias alu="apt list --upgradable"
	alias aulu="sudo apt update && apt list --upgradable"

	# act on the repos
	alias as="sudo apt search"
	alias ash="sudo apt show"
	alias adl="apt download" # gets source .deb in current directory

	# package handling
	alias aac="sudo apt autoclean"
	alias aar="sudo apt autoremove"
	alias ama="sudo apt-mark auto"
	alias amm="sudo apt-mark manual"
fi

if [ -x /usr/bin/dpkg ]; then
	alias dgl='dpkg --listfiles' # target a package name, e.g. dgl bspwm
	alias dgg='dpkg --get-selections' # would normally be pipped to grep
	# The following removes/purges unused configs without asking for
	# confirmation.  Same end product as 'alias apc' (see below where
	# aptitude is defined).
	alias dgp='sudo dpkg --purge $(dpkg --get-selections | grep deinstall | cut -f 1)'
fi

if [ -x /usr/bin/aptitude ]; then
	# The following two aliases perform the same action of removing
	# unused system files.  Unlike 'alias dgp', confirmation is needed.
	#alias apc="sudo aptitude purge ?config-files"
	alias apc="sudo aptitude purge ~c"
fi

# }}}

# Common tasks and utilities {{{
# ------------------------------

# Check these because some of them modify the behaviour of standard
# commands, such as `cp`, `mv`, `rm`, so that they provide verbose
# output and open a prompt when an existing file is affected.

# Entering Vim is made easier!
if [ -x /usr/bin/vim ]; then
    alias v='vim'
    alias vi='vim'
fi

# Same as above, though I currently do not use Emacs.
if [ -x /usr/bin/emacs ]; then
    alias e='emacsclient -c'
fi

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

# Some common tasks for the `rsync` utiity.
alias rsync='rsync --progress'
alias rsyncavz='rsync -avz --progress'
alias rsyncavzr='rsync -avzr --progress'
alias rsyncavzrd='rsync -avzr --delete --progress'

# Enable automatic color support for common commands that list output
# and also add handy aliases.  Note the link to the `dircolors`.  This
# is provided by my dotfiles.
if [ -x /usr/bin/dircolors ]; then
	dircolors_data="$HOME/.local/share/my_bash/dircolors"
    test -r $dircolors_data && eval "$(dircolors -b ${dircolors_data})" || eval "$(dircolors -b)"
fi

alias dir='dir --color=auto'
alias vdir='vdir --color=auto'

# TODO document common offenders to be excluded from grep -R such as:
# grep -R --exclude-dir='.git'
alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'

# Make ls a bit easier to read.  Note that the -A is the same as -a but
# does not include implied paths (the current dir denoted by a dot and
# the previous dir denoted by two dots).  I would also like to use the
# -p option, which prepends a forward slash to directories, but it does
# not seem to work with symlinked directories. For more, see `man ls`.
alias ls='ls --color=auto --group-directories-first'
alias lsa='ls -A --color=auto --group-directories-first'
alias lsl='ls -lh --color=auto --group-directories-first'
alias lsla='ls -lhA --color=auto --group-directories-first'

# Shortcuts to common configs.
# TODO review those.
alias bbb="$EDITOR ~/.bashrc"
alias ddd="$EDITOR ~/.config/dunst/dunstrc"
# alias mmm="$EDITOR ~/.neomuttrc"
# alias ppp="$EDITOR ~/.config/polybar/config"
# alias rrr="$EDITOR ~/.config/ranger/rc.conf"
# alias sss="$EDITOR ~/.neomutt/mails/signature"
# alias vvv="$EDITOR ~/.vimrc"
# alias xxx="$EDITOR ~/.Xresources"

alias urls="$EDITOR ~/.config/newsboat/urls" # changed this because `nnn` is an executable

# reload config files
alias rbbb='source ~/.bashrc'
alias rddd='killall dunst && eval $(dbus-launch)'
# alias rxxx='xrdb -merge ~/.Xresources'
#
# }}}

# Extra tasks and infrequently used tools {{{
# -------------------------------------------

# Profile switching in `gnome-terminal`.  I define two profiles, named
# Tempus Light/Dark.  Then these commands open a new tab with the
# desired profile.  In other words, one tab can be dark, the other
# light.  I do not actually use `gnome-terminal` but this is a gimmick I
# found and could be useful in some contexts.  As for "Tempus", see
# https://protesilaos.com/tempus-themes.  These are also integrated into
# my dotfiles, though not for `gnome-terminal`.
if [ -x /usr/bin/gnome-terminal ]; then
    alias gterml="gnome-terminal --tab-with-profile='Tempus Light'"
    alias gtermd="gnome-terminal --tab-with-profile='Tempus Dark'"
fi

# Used in place of w3m when the latter fails to display content
# correctly.  These options are very opinionated, disabling images,
# javascripts, etc.  See `man surf`.
if [ -x /usr/bin/surf ]; then
    alias surf="surf -giKMnps"
fi

# Quick shortcuts for `mpv`.  When I want to play a podcast that only
# shows a static image, I run the command with the --no-video option.
# When I only need to view the file I use --no-audio.  By the way, `mpv`
# is able to stream content directly from the web, thanks to
# `youtube-dl`.
if [ -x /usr/bin/mpv ]; then
    alias mpvna='mpv --no-audio'
    alias mpvnv='mpv --no-video'
fi

# Quick shortcuts for `youtube-dl`.  Note the paths where the files are
# extracted to.  The first places the output inside the present working
# directory.
if [ -x /usr/bin/youtube-dl ]; then
    alias ytaud='youtube-dl --add-metadata -ci --extract-audio --audio-format mp3 -o "%(title)s.%(ext)s"'
    alias ytvid='youtube-dl --add-metadata --no-playlist --no-part --write-description --newline --prefer-free-formats -o "%(title)s.%(ext)s" '
fi

# Certbot.  This is a utility that handles Let's Encrypt certificates
# for https connections.
if [ -x /usr/bin/certbot ]; then
    alias certm='sudo certbot certonly -a manual -d'
fi

# Get external/public IP address.
alias mypubip='curl ipinfo.io/ip'

# When I need to copy the contents of a file to the clipboard
if [ -x /usr/bin/xclip ]; then
	alias xclipc='xclip -selection clipboard' # followed by path to file
fi

# }}}

# Flatpak commands {{{
# --------------------

if [ -x /usr/bin/flatpak ]; then
    alias fli="flatpak install" # must be followed by a source, e.g. fli flathub
    alias fliu="flatpak uninstall"
    alias flls="flatpak list"
    alias flu="flatpak update"
fi

# }}}

# Git commands {{{
# ----------------

if [ -x /usr/bin/git ]; then
    # status
    alias gsta='git status'
    alias gstat='git status'

    # add, commit
    alias gadd='git add -v'
    alias gaddi='git add --interactive'
    alias gall='git add -Av'
    alias gcom='git commit -S -m' # gpg sign

    # amend
    alias gca='git commit --amend'
    alias grh='git reset HEAD'

    # diff
    alias gdif='git diff'
    alias gdiff='git diff'
    alias gdifs='git diff --stat --summary'
    alias gdiffss='git diff --stat --summary'

    # branching
    alias gch='git checkout'
    alias gchb='git checkout -b'
    alias gbd='git branch -d'
    alias gbl='git branch --list'
    alias gpd='git push origin --delete'

    # syncing
    alias gpull='git pull'
    alias gfetch='git fetch'
    alias gpm='git push -u origin master'
    alias gph='git push -u origin HEAD'
fi

# }}}

# Web dev tools {{{
# -----------------

# ruby bundler (jekyll)
if [ -x /usr/bin/bundler ]; then
    alias bibu='bundle install --path vendor/bundle && bundle update'
    alias bu='bundle update'
    alias bejs='bundle exec jekyll serve'
    alias bejsdev='bundle exec jekyll serve --config _config.yml,_config-dev.yml'
fi

# npm (node package manager)
# TODO add aliases for other npm tasks.
# TODO consider adding shortcuts for gulp
if [ -x /usr/bin/npm ]; then
    alias npmiu='npm install && npm update'
fi

# }}}

# }}}

# Functions {{{
# =============

# Colourise man pages
man() {
    env \
    LESS_TERMCAP_mb=$(tput bold; tput setaf 5) \
    LESS_TERMCAP_md=$(tput bold; tput setaf 5) \
    LESS_TERMCAP_me=$(tput sgr0) \
    LESS_TERMCAP_se=$(tput rmso; tput sgr0) \
    LESS_TERMCAP_ue=$(tput rmul; tput sgr0) \
    LESS_TERMCAP_so=$(tput bold; tput setaf 0; tput setab 15) \
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
cd() {
    if [ -n "$1" ]; then
        builtin cd "$@" && ls -A --color=auto --group-directories-first 
    else
        builtin cd ~ && ls -A --color=auto --group-directories-first
    fi
}

# Back up a file. Usage "backupthis <filename>"
backupthis() {
    cp -riv $1 ${1}-`date +%Y%m%d%H%M`.backup;
}

# Playerctl display album art (tested with mpd and well-structured Music metadata).
# dependencies:
#   sudo apt install playerctl sxiv
albumart() {
    if [ -x /usr/bin/playerctl ]; then
        local albumcover=$(playerctl metadata 'mpris:artUrl' | sed 's,file://,,g')
    fi

    if [ -x /usr/bin/sxiv ]; then
        if [ "$XDG_SESSION_DESKTOP" == 'bspwm' ]; then
            sxiv -g '150x150-15+15' -b -p -N my_float_window "$albumcover"
        else
            sxiv -g '150x150-15+15' -b -p  "$albumcover"
        fi
    fi
}

# }}}

# vi:foldmethod=marker
