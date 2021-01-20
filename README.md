# My Emacs and other configurations for GNU/Linux

This repository is managed with the help of GNU Stow.  What that program
does is create and manage symlinks from a target directory to a
destination.  For example:

```sh
$ git clone --depth 1 https://gitlab.com/protesilaos/dotfiles.git ~/prot-dotfiles
$ cd ~/prot-dotfiles
~/prot-dotfiles $ stow -t "$HOME" emacs
```

The above will clone my dotfiles to your `~/prot-dotfiles` path.  Then
`stow` will produce a symlink of my Emacs files to your home directory.
The target is called a "package" in Stow's manual---make sure to always
read the docs.

Other common `stow` operations are:

```sh
# Re-install stow "package"
~/prot-dotfiles $ stow -t "$HOME" -R emacs

# Delete stow package
~/prot-dotfiles $ stow -t "$HOME" -D emacs
```

Here is a sample of my Emacs files (may not represent the actual file
structure).

```sh
~/prot-dotfiles $ tree -aF emacs
emacs
└── .emacs.d/
    ├── early-init.el
    ├── emacs-init.org
    ├── init.el
    └── straight/
        └── repos/
            ├── modus-themes/
            │   ├── modus-operandi-theme.el
            │   ├── modus-themes.el
            │   └── modus-vivendi-theme.el
            └── prot-lisp/
                ├── prot-bongo.el
                ├── prot-common.el
                ├── prot-consult.el
                ├── prot-diff.el
                ├── prot-dired.el
                ├── prot-elfeed-bongo.el
                ├── prot-elfeed.el
                ├── prot-embark.el
                ├── prot-embark-extras.el
                ├── prot-eshell.el
                ├── prot-fonts.el
                ├── prot-ibuffer.el
                ├── prot-icomplete.el
                ├── prot-logos.el
                ├── prot-minibuffer.el
                ├── prot-moody.el
                ├── prot-orderless.el
                ├── prot-outline.el
                ├── prot-project.el
                ├── prot-pulse.el
                ├── prot-recentf.el
                ├── prot-search.el
                ├── prot-simple.el
                ├── prot-tab.el
                ├── prot-text.el
                ├── prot-vc.el
                ├── tmr.el
                └── usls.el

5 directories, 34 files
```

## Do not track my dotfiles (rolling and unstable)

_Ceci n'est pas une distribution Emacs._ (if you know what I mean)

The above granted, note that this repo functions as a laboratory of
experimentation for my computing environment.  What I do with Emacs or
other programs in the GNU/Linux milieu is only meant to work for me.  As
such, I offer no support whatsoever to those tracking my dotfiles and
may introduce breaking changes without prior notice.

This is all to say that **you understand the risks of tracking an
ever-changing project**.  If you are fine with that and are willing to
assume responsibility for any possible breakage, then please feel
welcome to follow along.  You can always open an issue here or
contribute any fixes, if you will.

## Old BSPWM configuration

**DEPRECATION NOTICE 2020-05-04.** I am no longer using BSPWM and
several of its accoutrements.  My focus is on my comprehensive Emacs
setup and whatever external tweak may be necessary to improve the
experience with that tool.

The last state of my dotfiles with BSPWM and its extras can be found in
[tag v2.2.0](https://gitlab.com/protesilaos/dotfiles/-/tree/v2.2.0).  I
provide no change logs, no support (see the README of that tag for more
information).

## Copying

Unless otherwise noted, all code herein is distributed under the terms
of the GNU General Public License Version 3 or later.
