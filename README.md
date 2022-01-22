# My Emacs and other configurations for Arch Linux

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

## Window managers

As of 2021-11-08, I have configurations for three window managers, in
order from oldest to newest:

+ **bspwm:** I used this for years.  In the summer of 2021 I configured
  it again to bring it up to standard.  Some legacy issues remain
  (specifically with the key bindings), but it is okay overall.  Use
  this if you want something that is easy to grasp for beginners, but
  super-powerful for those who wish to script its behaviour.

+ **swaywm:** I used it for about two weeks at the end of September or
  the beginning of October 2021.  I did it mostly because I was curious
  about Wayland.  Overall, things work fine and I am happy with it.  I
  don't use the tabbed/stacked layouts and don't mess around too much
  with its container functionality for grouping windows.  Just give me
  the standard tiling scheme.  If/when Emacs' `pgtk` branch is merged
  into `master`, I might reconsider it.  In the meantime, I stay with
  Xorg.  Choose Sway if you want to switch to Wayland right away.

+ **herbstluftwm:** I used this WM privately some years ago when I was
  deciding between it and bspwm.  If you check and compare all relevant
  configurations, you will notice that herbstluftwm and bspwm are very
  similar in how they work.  The latter has an automatic tiling scheme
  that is smart enough to split windows depending on their direction or
  longest side, whereas the former is strictly manual.  The main
  difference is that herbstluftwm handles monitors and workspaces
  ("tags") differently.  In short: a monitor can be virtual, by being
  assigned to a portion of the effective screen area.  Opt for
  herbstluftwm if you have an ultrawide display and want to create
  virtual monitors out of it.  I got such a display on 2021-11-08 as
  part of a temporary exchange with a friend and my first impressions
  are positive.  Too bad I won't keep it for too long...

Sway's configurations are self-contained.  Whereas bspwm and
herbstluftwm have a shared basis in the `xorg-twm` stow package.  I plan
to document everything: (i) how to use stow, (ii) how the directory
strucure is designed, (iii) package names and dependencies.  In the
meantime, **this is highly experimental and you should not use anything,
unless you know what you are doing.**

## How to reproduce my Emacs setup

I am not using `use-package` so this will not be an easy ride.  It is
better for you to peruse the relevant files and copy whatever you want
instead of reproducing the whole thing.

If, however, you insist on going ahead with your plan, I provide more
details in the heading _"How to reproduce your dotemacs?"_ of my
`prot-emacs.org`.  Either find it there, or read it on my website:
<https://protesilaos.com/emacs/dotemacs/#h:0675f798-e2d9-4762-9df2-f47cd24cf00a>.

It also explains how to manage my dotfiles with the help of GNU Stow,
notwithstanding the need for proper documentation, as noted in the
previous section.

## Copying

Unless otherwise noted, all code herein is distributed under the terms
of the GNU General Public License Version 3 or later.
