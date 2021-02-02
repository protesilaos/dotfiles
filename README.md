# My Emacs and other configurations for GNU/Linux

This is the set of files that powers my day-to-day computing experience.
GNU Emacs is its centrepiece, with other programs providing ancillary
support, where relevant.  In some cases, such as with the configurations
for Vim and Xterm, all relevant files are maintained from my pre-Emacs
days and are preserved as a contingency plan.
 
## Do not track my dotfiles (rolling, unstable, and untested)

_Ceci n'est pas une distribution Emacs._ If you know what I mean.

This repo functions as a laboratory of experimentation for my computing
environment.  What I do with Emacs or other programs in the GNU/Linux
milieu is only meant to work for me.  As such, I offer no support
whatsoever to those tracking my dotfiles and may introduce breaking
changes without prior notice.

This is all to say that **you understand the risks associated with
tracking an ever-changing project that does not enjoy widespread testing
and whose target audience is only me**.  If you are fine with that and
are willing to assume responsibility for any possible breakage, then
please feel welcome to follow along.  You can always open an issue here
or contribute any fixes, if you will.

## Deployment with GNU Stow

This repository is managed with the help of GNU Stow.  What that program
does is create and manage symlinks from a target directory to a
destination.  The file structure of this repo is thus designed to
reflect the expected end result on a `$HOME` directory.

For example (the `-v` flag is for verbose output):

```sh
$ git clone --depth 1 https://gitlab.com/protesilaos/dotfiles.git ~/prot-dotfiles
$ cd ~/prot-dotfiles
~/prot-dotfiles $ stow -v -t "$HOME" emacs
LINK: .emacs.d/early-init.el => ../Git/Projects/dotfiles/emacs/.emacs.d/early-init.el
LINK: .emacs.d/prot-emacs.org => ../Git/Projects/dotfiles/emacs/.emacs.d/prot-emacs.org
LINK: .emacs.d/modus-themes => ../Git/Projects/dotfiles/emacs/.emacs.d/modus-themes
LINK: .emacs.d/prot-lisp => ../Git/Projects/dotfiles/emacs/.emacs.d/prot-lisp
LINK: .emacs.d/init.el => ../Git/Projects/dotfiles/emacs/.emacs.d/init.el
```

Please understand what link creation means in this context: if the files
in the home directory already exist, then `stow` will report a warning
and abort its operations.  As such, **always back up your files** and
please remember that **this is not an Emacs distro**.

Those granted, the above commands will first shallow clone my dotfiles
to your `~/prot-dotfiles` path ("shallow" means to only fetch the Nth
`--depth` commit instead of the entire history, so just the latest one
in our example).  Then `stow` will make symbolic links of my Emacs files
to the home directory.

The argument passed to `stow` is called a "package" in the program's
manual---make sure to always read the docs.  All subdirectories of my
dotfiles, like `fontconfig` and `shell`, are such "packages".

Other common `stow` operations for the purposes of managing this
repository:

```sh
# Re-install stow package
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
    ├── init.el
    ├── modus-themes/
    │   ├── modus-operandi-theme.el
    │   ├── modus-themes.el
    │   └── modus-vivendi-theme.el
    ├── prot-emacs.org
    └── prot-lisp/
        ├── prot-bongo.el
        ├── prot-comment.el
        ├── prot-common.el
        ├── prot-consult.el
        ├── prot-diff.el
        ├── prot-dired.el
        ├── prot-elfeed-bongo.el
        ├── prot-elfeed.el
        ├── prot-embark.el
        ├── prot-embark-extras.el
        ├── prot-eshell.el
        ├── prot-fill.el
        ├── prot-fonts.el
        ├── prot-gnus.el
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
        ├── prot-spell.el
        ├── prot-tab.el
        ├── prot-text.el
        ├── prot-vc.el
        ├── tmr.el
        └── usls.el

3 directories, 38 files
```

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
