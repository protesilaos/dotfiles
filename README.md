Custom desktop session (BSPWM) and configurations for GNU/Linux
===============================================================

My custom desktop session can be outlined thus:

* Debian stable because I prefer longer-term predictability over
  novelty.
* `bspwm` for fine-grained window management with my custom scripts for
  added features.
* `tmux` to make the terminal power user even more powerful (no
  plugins).
* `vim` because I need an efficient text editor (no plugins).
* True minimalism: no complex status lines, no fancy prompts, no
  decorative elements that add no functionality whatsoever.
* Carefully-defined font configurations to complement my hardware and
  satisfy my aesthetic preferences.
* Full integration of my Tempus themes for considerate colour contrast
  and easy live theme switching for the entire session.
* The superb Maté desktop environment as a fallback option and provider
  of some important programs and system-wide base configurations.

Demos
-----

* Screen Casts: https://protesilaos.com/code-casts

Get my configs
---------------

To reproduce my custom desktop session on Debian 10 'buster', follow the
detailed instructions in my book "Prot's Dots for Debian (PDFD)":
https://protesilaos.com/pdfd.  It is freely available under the terms of
Creative Commons Attribution-ShareAlike License Version 4.0
International.

PDFD will only offer guidance about the latest "fixed release" of my
dotfiles.  These are tagged with the appropriate annotation (formatted
as <version>-pdfd<?\_label>) and are then synced with a separate code
repo, titled "Code for PDFD": https://gitlab.com/protesilaos/cpdfd.

Do not track my dotfiles directly
---------------------------------

My dotfiles' repo functions as a laboratory for my custom desktop
session.  I offer no support whatsoever about tracking it and may
introduce breaking changes without prior notice.

Just use PDFD.  It is why I spent an inordinate amount of time cleaning
up the code, documenting it extensively, and ultimately writing
instructions about how to reproduce it.

That granted, you can always open an issue here or contribute any fixes.

Copying
-------

Unless otherwise noted, all code herein is distributed under the terms
of the GNU General Public License Version 3 or later.
