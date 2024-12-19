# My Emacs and other configurations for Linux

This is the set of files that powers my day-to-day computing experience.
GNU Emacs is its centrepiece, with other programs providing ancillary
functionalities.  In some cases, such as with the configurations for
Vim, all relevant files are carried over from my pre-Emacs days and are
maintained as a contingency plan or in case I need to revisit some old
setup.

## Do not track my dotfiles (rolling, unstable, and untested)

_Ceci n'est pas une distribution Emacs._

This repo functions as a laboratory of experimentation for my computing
environment.  What I do with Emacs or any other program in the GNU/Linux
milieu that forms part of my dotfiles is only meant to work for me.  As
such, I offer no support whatsoever to those tracking this repository
and may introduce breaking changes without prior notice.

This is all to say that **you understand the risks associated with
tracking an ever-changing project that does not enjoy widespread testing
and whose target audience is only me**.  If you are fine with that and
are willing to assume responsibility for any possible breakage, then
please feel welcome to follow along.  You can always open an issue here
or contribute any fixes, if you will.

## Emacs setup

I do not recommend you reproduce my Emacs setup because I do not
design it as a distribution for other people. If you insist though,
the files are in the `emacs` directory. Add them to your home
directory with:

```sh
/path/to/prot-dotfiles $ stow -t "$HOME" emacs
```

This will create symlinks to my configuration files inside the
`~/.emacs.d` directory.  My custom libraries are in the directory
`prot-lisp` while the configuration modules (where we tweak variables,
assign key bindings, etc.) are in the directory `prot-emacs-modules`.

The modules are loaded from the `init.el`.  Each module defines the
packages to install/load.  **My setup auto-installs packages**.  This
will happen the first time you start up Emacs.

Read the file `prot-emacs.org` for further information on the anatomy
of my Emacs setup.

If you make changes to the dotfiles, such as by moving things around,
run stow again with the `-R` flag:

```sh
/path/to/prot-dotfiles $ stow -t "$HOME" -R emacs
```

## Window managers

My tiling window managers for Xorg are bspwm, herbstluftwm, and i3 (in
no particular order). They are all configured to be almost the same:
they share settings for the wallpaper, system panel, theme, keyboard
layout, and display compositor. All settings are in the `xorg-twm`
directory ("twm" stands for "tiling window manager"). What
differentiates the window managers is their individual features.

+ **bspwm:** I have been using it for years and consider it top-notch.
  It is minimal, stable, and scriptable.  Use this if you prefer
  automatic tiling.  It does not have layout features out-of-the-box,
  such as a tabbed layout: it can place windows in tiles or float
  them.

+ **herbstluftwm (hlwm):** herbstluftwm prioritises manual tiling
  methods. It has the concept of "frames", which are rectangles that
  include regular app windows. Each frame has its own dimensions are
  layout. For example, it is possible to have a frame where windows
  are vertically layed out one below the other and another frame with
  a tabbed layout. What makes this even more appealing, is the ease
  with which a workspace can be preconfigured (or dymanically
  adjusted) to a given arrangement of frames and their layouts. I
  think herbstluftwm is optimal for screens wider than the the regular
  1920x1080. Otherwise, the manual tiling method adds cognitive load
  that the user does not have while using bspwm.

+ **i3 (or i3wm):** The first tiling window manager I ever used (circa
  2017). The reason I abandoned it back in the day in favour of bspwm
  is because its default tiling method requires manual intervention to
  change the split direction. On a small laptop monitor, I prefer this
  to be done automatically, hence bspwm. Though on a wide monitor, I
  typically keep the split direction constant. i3 has the concept of
  the "container" (same as the herbstluftwm "frame"), which can be set
  to stacked, tabbed, or tiled layouts. As such, i3 is somewhere
  between bspwm and herbstluftwm. Choose according to your needs.

Check the `xtwm-key-binding-cheatsheet.md` file for an overview of
their key bindings.

When I have a video call or record a video of my desktop session, I
always use herbstluftwm because of its ability to support virtual
monitors.  Otherwise I oscillate between bspwm and i3, depending on
the machine I am using.

What about Wayland? I have experimented with it on GNOME where it
works fine. I even have some configurations for tiling compositors,
but am not happy with those. The Wayland ecosystem is not ready yet
for my use-case: I am missing something like sxhkd and certain
applications do not work properly on it, such screen sharing. I will
check again some time over the medium term, as Wayland seems
inevitable.

## Copying

Unless otherwise noted, all code herein is distributed under the terms
of the GNU General Public License Version 3 or later.
