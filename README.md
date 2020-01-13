My BSPWM and Emacs for GNU/Linux
================================

**UPDATE 2020-01-13:** I continue to use Debian 10 'buster' (current
stable) as my main operating system.  Check the tagged releases for the
commits that were made from Void Linux box.  Going forward, I will be
using Debian as my benchmark, because it is easier for maintenance: a
rolling-release distro requires too much effort to keep track of all
changes and to update all relevant documentation accordingly (plus, I
prefer a stable system overall).

I plan to stick with BSPWM at least for the entire life-cycle of Debian
10, even though my focus now is on Emacs.

Get my old configs
------------------

To reproduce _the previous iteration_ of my custom desktop session on
Debian 10 'buster', follow the detailed instructions in my book "Prot's
Dots for Debian" (PDFD): https://protesilaos.com/pdfd.  It is freely
available under the terms of the Creative Commons Attribution-ShareAlike
License, Version 4.0 International.

The major difference between the present unstable state and the one in
PDFD is that now I am making changes to accommodate Emacs.  Before that
I was using Tmux+Vim together with other standalone CLI programs.  There
also are some refinements on the scripts I have written and the general
shell or desktop environment.

PDFD will only offer guidance about the latest stable state of my
dotfiles.  The code has been forked from this repository and is
maintained as a separate project _for as long as Debian 10 'buster' is
the current stable version_: https://gitlab.com/protesilaos/cpdfd

**As such, PDFD will never contain the latest breaking changes.** Please
contact me if you have any questions: https://protesilaos.com/contact/

Do not track my dotfiles directly
---------------------------------

My dotfiles' repo functions as a laboratory for my custom desktop
session.  I offer no support whatsoever about tracking it and may
introduce breaking changes without prior notice.

Just use PDFD.  It is why I spent an inordinate amount of time cleaning
up the code, documenting it extensively, and ultimately writing
instructions on how to reproduce it.

That granted, you can always open an issue here or contribute any fixes.

Copying
-------

Unless otherwise noted, all code herein is distributed under the terms
of the GNU General Public License Version 3 or later.
