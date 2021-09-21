# My Emacs and other configurations for GNU/Linux

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

## How to reproduce my Emacs setup

I am not using `use-package` so this will not be an easy ride.  It is
better for you to peruse the relevant files and copy whatever you want
instead of reproducing the whole thing.

If, however, you insist on going ahead with your plan, I provide more
details in the heading _"How to reproduce your dotemacs?"_ of my
`prot-emacs.org`.  Either find it there, or read it on my website:
<https://protesilaos.com/dotemacs/#h:0675f798-e2d9-4762-9df2-f47cd24cf00a>.

It also explains how to manage my dotfiles with the help of GNU Stow.

## BSPWM configuration

As of 2021-08-27 I am back to using BSPWM.  Emacs remains the
centrepiece of my computing environment, though I got bored of Xfce and
thought I would re-use my old configurations.  There have been some
updates and more will follow.  Everything should be considered a
work-in-progress until further notice.

## Experiments with Sway

Starting on 2021-09-21, I maintain configuration related to the Sway
window manager.  The plan is to switch to Wayland at some point in the
near future.  Sway is a stable package for testing things, though I am
not sure I will stick with it.  Perhaps there is another window manager
I might like more.  In the meantime, **consider everything related to
Sway to be highly experimental, unstable, and subject to removal without
prior notice**.

## Copying

Unless otherwise noted, all code herein is distributed under the terms
of the GNU General Public License Version 3 or later.
