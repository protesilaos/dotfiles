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

## Emacs setup

I do not recommend you reproduce my Emacs setup because I do not use the
de facto `use-package` to configure packages.  I prefer a simpler
approach.

If you insist though, the files are in the `emacs` directory.  Add them
to your home directory with:

```sh
/path/to/prot-dotfiles $ stow -t "$HOME" emacs
```

This will create symlinks to my configuration files inside the
`~/.emacs.d` directory.  My custom libraries are in the directory
`prot-lisp` while the configuration modules (where we tweak variables,
assign key bindings, etc.) are in the directory `prot-emacs-modules`.

The modules are loaded from the `init.el`.  Each module defines the
packages to install/load.  **My setup auto-installs packages**.  This
will happen the first time you start up Emacs.  In particular, the
`prot-emacs-elpa-package` macro performs that task.  You will find that
macro in all of the modules: it is a thin wrapper which runs `require`
on the given package as well as evaluates all code inside of it (so you
don't have to evaluate each form individually).  Example:

```elisp
(prot-emacs-elpa-package 'cursory
  (setq cursory-presets
        '((bar
           :cursor-type (bar . 2)
           :cursor-in-non-selected-windows hollow
           :blink-cursor-blinks 10
           :blink-cursor-interval 0.5
           :blink-cursor-delay 0.2)
          (box
           :cursor-type box
           :cursor-in-non-selected-windows hollow
           :blink-cursor-blinks 10
           :blink-cursor-interval 0.5
           :blink-cursor-delay 0.2)
          (underscore
           :cursor-type (hbar . 3)
           :cursor-in-non-selected-windows hollow
           :blink-cursor-blinks 50
           :blink-cursor-interval 0.2
           :blink-cursor-delay 0.2)))

  (setq cursory-latest-state-file (locate-user-emacs-file "cursory-latest-state.eld"))

  ;; Set last preset or fall back to desired style from `cursory-presets'.
  (cursory-set-preset (or (cursory-restore-latest-preset) 'bar))

  ;; The other side of `cursory-restore-latest-preset'.
  (add-hook 'kill-emacs-hook #'cursory-store-latest-preset)

  ;; We have to use the "point" mnemonic, because C-c c is often the
  ;; suggested binding for `org-capture'.
  (define-key global-map (kbd "C-c p") #'cursory-set-preset))
```

If a package is not found in the archives it likely means that you need
to refresh the package listing: `M-x package-refresh-contents`.  This is
done automatically at startup, if necessary, but is needed for any new
packages you may define.  Then retry installing the package.

The macro `prot-emacs-builtin-package` simply is a wrapper.  It does not
install anything, as it is for libraries that are built into Emacs OR
the code in my `prot-lisp` directory.

If you make changes to the dotfiles, such as by moving things around,
run stow again with the `-R` flag:

```sh
/path/to/prot-dotfiles $ stow -t "$HOME" -R emacs
```

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
  into `master`, I might reconsider it (UPDATE 2022-04-24 08:37 +0300:
  Emacs 29 can be built `--with-pgtk` but I still prefer bspwm and the
  Xorg build of Emacs).  In the meantime, I stay with Xorg.  Choose Sway
  if you want to switch to Wayland right away.

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

### TODO for window managers

As of 2022-04-24.  In no particular order.

+ A herbstluftwm script like `bspwm_resize`.
+ Same as above for sway.
+ Can herbstluftwm mouse drag/resize be defined in sxhkd?
+ Use a bold weight for the focused workspace in sway's bar?  Add other
  characters like `[1]` for the focused workspace, `2^` for unfocused
  but occupied, and `3` for unfocused and empty? (That is how I do it
  with polybar on bspwm/herbstluftwm.)
+ Show keyboard layout in sway bar (English/Greek).

## Copying

Unless otherwise noted, all code herein is distributed under the terms
of the GNU General Public License Version 3 or later.
