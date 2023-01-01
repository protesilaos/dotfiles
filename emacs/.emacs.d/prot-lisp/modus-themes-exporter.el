;;; modus-themes-exporter.el --- Help port the active Modus theme to external applications -*- lexical-binding:t -*-

;; Copyright (C) 2021-2023  Protesilaos Stavrou

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://git.sr.ht/~protesilaos/modus-themes
;; Version: 0.0.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: faces, theme, accessibility

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; EXPERIMENTAL AND ONLY INTENDED FOR ADVANCED USERS
;;
;; Helper functions to produce a port of the currently
;; enabled Modus theme (`modus-operandi' or `modus-vivendi') for various
;; terminal emulators or text editors outside the Emacs milieu.
;;
;; Insofar as external text editors are concerned, such ports are not
;; meant to be treated as 'official themes'.  This is a mere facility to
;; approximate the surface-level aesthetics of the Modus themes: a real
;; theme requires attention to detail; a requirement that no generic
;; template can satisfy.
;;
;; Consult the doc string of `modus-themes-exporter-export' for how to
;; use this library.
;;
;; Also read "Introducing the Modus themes exporter (Emacs library)":
;; <https://protesilaos.com/codelog/2021-02-22-modus-themes-exporter/>

;;; Code:



(if (functionp 'require-theme)
    (require-theme 'modus-themes) ; for those provided by Emacs >= 28
  (require 'modus-themes))
(require 'dom)
(require 'seq)

(defun modus-themes-exporter--current-theme ()
  "Return current modus theme."
  (car
   (seq-filter
    (lambda (theme)
      (string-match-p "^modus" (symbol-name theme)))
    custom-enabled-themes)))

(defvar modus-themes-exporter-templates-alist
  '(("alacritty"        . modus-themes-exporter-alacritty)
    ("foot"             . modus-themes-exporter-foot)
    ("iterm2"           . modus-themes-exporter-iterm2)
    ("urxvt"            . modus-themes-exporter-urxvt)
    ("vim"              . modus-themes-exporter-vim)
    ("windows-terminal" . modus-themes-exporter-windows-terminal)
    ("xcolors"          . modus-themes-exporter-xcolors)
    ("xfce"             . modus-themes-exporter-xfce)
    ("xterm"            . modus-themes-exporter-xterm)))

;;;; Generic Xcolors template

(defun modus-themes-exporter-xcolors ()
  "Template for generic Xcolors."
  (modus-themes-with-colors
    (let ((theme-name (format "%s" (modus-themes-exporter--current-theme))))
      (with-temp-buffer
        (concat
         "! Theme: " theme-name "\n"
         "! Description: Generic Xcolors port of " theme-name " (Modus themes for Emacs)" "\n"
         "! Author: Protesilaos Stavrou, <https://protesilaos.com>" "\n"
         "*background: " bg-main "\n"
         "*foreground: " fg-main "\n"
         "*color0: " "#000000" "\n"
         "*color1: " red "\n"
         "*color2: " green "\n"
         "*color3: " yellow "\n"
         "*color4: " blue "\n"
         "*color5: " magenta "\n"
         "*color6: " cyan "\n"
         "*color7: " "#bfbfbf" "\n"
         "*color8: " "#595959" "\n"
         "*color9: " red-alt "\n"
         "*color10: " green-alt "\n"
         "*color11: " yellow-alt "\n"
         "*color12: " blue-alt "\n"
         "*color13: " magenta-alt-other "\n"
         "*color14: " cyan-alt-other "\n"
         "*color15: " "#ffffff" "\n")))))

;;;; XTerm template

(defun modus-themes-exporter-xterm ()
  "Template for XTerm."
  (modus-themes-with-colors
    (let ((theme-name (format "%s" (modus-themes-exporter--current-theme))))
      (with-temp-buffer
        (concat
         "! Theme: " theme-name "\n"
         "! Description: XTerm port of " theme-name " (Modus themes for Emacs)" "\n"
         "! Author: Protesilaos Stavrou, <https://protesilaos.com>" "\n"
         "xterm*background: " bg-main "\n"
         "xterm*foreground: " fg-main "\n"
         "xterm*color0: " "#000000" "\n"
         "xterm*color1: " red "\n"
         "xterm*color2: " green "\n"
         "xterm*color3: " yellow "\n"
         "xterm*color4: " blue "\n"
         "xterm*color5: " magenta "\n"
         "xterm*color6: " cyan "\n"
         "xterm*color7: " "#bfbfbf" "\n"
         "xterm*color8: " "#595959" "\n"
         "xterm*color9: " red-alt "\n"
         "xterm*color10: " green-alt "\n"
         "xterm*color11: " yellow-alt "\n"
         "xterm*color12: " blue-alt "\n"
         "xterm*color13: " magenta-alt-other "\n"
         "xterm*color14: " cyan-alt-other "\n"
         "xterm*color15: " "#ffffff" "\n")))))

;;;; Alacritty template

(defun modus-themes-exporter-alacritty ()
  "Template for Alacritty."
  (modus-themes-with-colors
    (let ((theme-name (format "%s" (modus-themes-exporter--current-theme))))
      (with-temp-buffer
        (concat
         "# Theme: " theme-name "\n"
         "# Description: Alacritty port of " theme-name " (Modus themes for Emacs)" "\n"
         "# Author: Protesilaos Stavrou, <https://protesilaos.com>" "\n"
         "colors:" "\n"
         "  primary:" "\n"
         "    background: '" bg-main "'" "\n"
         "    foreground: '" fg-main "'" "\n"
         "  normal:" "\n"
         "    black:   '#000000'" "\n"
         "    red:     '" red "'" "\n"
         "    green:   '" green "'" "\n"
         "    yellow:  '" yellow "'" "\n"
         "    blue:    '" blue "'" "\n"
         "    magenta: '" magenta "'" "\n"
         "    cyan:    '" cyan "'" "\n"
         "    white:   '#bfbfbf'" "\n"
         "  bright:" "\n"
         "    black:   '#595959'" "\n"
         "    red:     '" red-alt "'" "\n"
         "    green:   '" green-alt "'" "\n"
         "    yellow:  '" yellow-alt "'" "\n"
         "    blue:    '" blue-alt "'" "\n"
         "    magenta: '" magenta-alt-other "'" "\n"
         "    cyan:    '" cyan-alt-other "'" "\n"
         "    white:   '#ffffff'" "\n")))))

;;;; Foot template

(defun modus-themes-exporter-foot ()
  "Template for Foot."
  (modus-themes-with-colors
    (let ((theme-name (format "%s" (modus-themes-exporter--current-theme))))
      (with-temp-buffer
        (string-replace
         "=#" "="
         (concat
          "# Theme: " theme-name "\n"
          "# Description: Foot port of " theme-name " (Modus themes for Emacs)" "\n"
          "# Author: Protesilaos Stavrou, <https://protesilaos.com>" "\n"
          "[colors]" "\n"
          "alpha=1.00" "\n"
          "background=" bg-main "\n"
          "foreground=" fg-main "\n"
          "regular0=000000" "\n"
          "regular1=" red "\n"
          "regular2=" green "\n"
          "regular3=" yellow "\n"
          "regular4=" blue "\n"
          "regular5=" magenta "\n"
          "regular6=" cyan "\n"
          "regular7=bfbfbf" "\n"
          "bright0=595959" "\n"
          "bright1=" red-alt "\n"
          "bright2=" green-alt "\n"
          "bright3=" yellow-alt "\n"
          "bright4=" blue-alt "\n"
          "bright5=" magenta-alt-other "\n"
          "bright6=" cyan-alt-other "\n"
          "bright7=ffffff" "\n"))))))

;;;; URxvt template

(defun modus-themes-exporter-urxvt ()
  "Template for URxvt (Rxvt-unicode)."
  (modus-themes-with-colors
    (let ((theme-name (format "%s" (modus-themes-exporter--current-theme))))
      (with-temp-buffer
        (concat
         "! Theme: " theme-name "\n"
         "! Description: URxvt port of " theme-name " (Modus themes for Emacs)" "\n"
         "! Author: Protesilaos Stavrou, <https://protesilaos.com>" "\n"
         "URxvt*background: " bg-main "\n"
         "URxvt*foreground: " fg-main "\n"
         "URxvt*cursorColor: " fg-main "\n"
         "URxvt*cursorColor2: " bg-main "\n"
         "URxvt*color0: " "#000000" "\n"
         "URxvt*color1: " red "\n"
         "URxvt*color2: " green "\n"
         "URxvt*color3: " yellow "\n"
         "URxvt*color4: " blue "\n"
         "URxvt*color5: " magenta "\n"
         "URxvt*color6: " cyan "\n"
         "URxvt*color7: " "#bfbfbf" "\n"
         "URxvt*color8: " "#595959" "\n"
         "URxvt*color9: " red-alt "\n"
         "URxvt*color10: " green-alt "\n"
         "URxvt*color11: " yellow-alt "\n"
         "URxvt*color12: " blue-alt "\n"
         "URxvt*color13: " magenta-alt-other "\n"
         "URxvt*color14: " cyan-alt-other "\n"
         "URxvt*color15: " "#ffffff" "\n")))))

;;;; Xfce terminal template

(defun modus-themes-exporter-xfce ()
  "Template for Xfce terminal."
  (modus-themes-with-colors
    (let ((theme-name (format "%s" (modus-themes-exporter--current-theme))))
      (with-temp-buffer
        (concat
"[Scheme]" "\n"
"Name=" theme-name "\n"
"ColorForeground=" fg-main "\n"
"ColorBackground=" bg-main "\n"
"ColorCursor=" fg-main "\n"
"ColorBold=" fg-main "\n"
"ColorPalette="
"#000000;" red ";" green ";" yellow ";" blue ";" magenta ";" cyan ";" "#bfbfbf;"
"#595959;" red-alt ";" green-alt ";" yellow-alt ";" blue-alt ";" magenta-alt-other ";" cyan-alt-other ";"
"#ffffff" "\n"
"TabActivityColor=" magenta-alt "\n"
"ColorSelectionBackground=" bg-region "\n"
"ColorSelection=" fg-main "\n")))))

;;;; Windows terminal template 

(defun modus-themes-exporter-windows-terminal ()
  "Template for Windows Terminal."
  (modus-themes-with-colors
    (let ((theme-name (format "%s" (modus-themes-exporter--current-theme))))
      (with-temp-buffer
        (concat
         "// Theme: " theme-name "\n"
         "// Description: Windows Terminal port of " theme-name " (Modus themes for Emacs)" "\n"
         "// Author: Protesilaos Stavrou, <https://protesilaos.com>" "\n"
         "\"name\": \"" theme-name "\",\n"
         "\"background\": \"" bg-main "\",\n"
         "\"foreground\": \"" fg-main "\",\n"
         "\"cursorColor\": \"" fg-main "\",\n"
         "\"selectionBackground\": \"" bg-region "\",\n"
         "\"black\": \"#000000\",\n"
         "\"red\": \"" red "\",\n"
         "\"green\": \"" green "\",\n"
         "\"yellow\": \"" yellow "\",\n"
         "\"blue\": \"" blue "\",\n"
         "\"purple\": \"" magenta "\",\n"
         "\"cyan\": \"" cyan "\",\n"
         "\"white\": \"#bfbfbf\",\n"
         "\"brightBlack\": \"#595959\",\n"
         "\"brightRed\": \"" red-alt "\",\n"
         "\"brightGreen\": \"" green-alt "\",\n"
         "\"brightYellow\": \"" yellow-alt "\",\n"
         "\"brightBlue\": \"" blue-alt "\",\n"
         "\"brightPurple\": \"" magenta-alt-other "\",\n"
         "\"brightCyan\": \"" cyan-alt-other "\",\n"
         "\"brightWhite\": \"#ffffff\"\n")))))

;;;; Vim template

;; Please skip this.  I only wrote it as an attempt to participate in
;; the contest for the ugliest concat ever imagined...  Seriously
;; though, we should come up with a more elegant solution for complex
;; templates.
(defun modus-themes-exporter-vim ()
  "Template for Vim."
  (modus-themes-with-colors
    (let* ((theme-name (format "%s" (modus-themes-exporter--current-theme)))
           (theme-name-underscore (string-replace "-" "_" theme-name))
           (variant (if (string= theme-name "modus-operandi") "light" "dark"))
           (termcolbg (format "%s" (if (string= variant "light") 15 0)))
           (termcolfg (format "%s" (if (string= variant "light") 0 15)))
           (termcolbgalt (format "%s" (if (string= variant "light") 7 8)))
           (termcolfgalt (format "%s" (if (string= variant "light") 8 7))))
      (with-temp-buffer
        (concat
"\" vi: ft=vim" "\n"
"\" Name: " theme-name-underscore "\n"
"\" Description: Vim port of " theme-name " (Modus themes for Emacs)" "\n"
"\" Author: Protesilaos Stavrou, <https://protesilaos.com>" "\n"
"\" Meta: Created with the modus-themes-exporter.el library" "\n"
"\" URL: https://git.sr.ht/~protesilaos/modus-themes" "\n"
"\n"
"set background=" variant "\n"
"hi clear" "\n"
"if exists('syntax_on')" "\n"
"	syntax reset" "\n"
"endif" "\n"
"let g:colors_name = " "'" theme-name-underscore "'" "\n"
"\n"
"\" General" "\n"
"\" -------" "\n"
"if exists('g:modus_themes_enforce_background_color')" "\n"
"	hi Normal guibg=" bg-main " guifg=" fg-main " ctermbg=" termcolbg " ctermfg=" termcolfg "" "\n"
"else" "\n"
"	\" NOTE the ctermbg=none is for terminals with transparency" "\n"
"	hi Normal guibg=" bg-main " guifg=" fg-main " ctermbg=none ctermfg=" termcolfg "" "\n"
"endif" "\n"
"" "\n"
"hi Visual guibg=" fg-main " guifg=" bg-main " ctermbg=" termcolfg " ctermfg=" termcolbg "" "\n"
"hi Search gui=underline,bold guibg=" bg-alt " guifg=" fg-main " cterm=underline,bold ctermbg=" termcolbgalt " ctermfg=3" "\n"
"hi IncSearch gui=underline,bold guibg=" fg-alt " guifg=" bg-main " term=none cterm=underline,bold ctermbg=" termcolfgalt " ctermfg=" termcolbg "" "\n"
"\n"
"hi StatusLine gui=none guibg=" fg-main " guifg=" bg-main " cterm=none ctermbg=" termcolfg " ctermfg=" termcolbg "" "\n"
"hi StatusLineNC gui=none guibg=" bg-alt " guifg=" fg-alt " cterm=none ctermbg=" termcolbgalt " ctermfg=" termcolfgalt "" "\n"
"hi StatusLineTerm gui=none guibg=" green " guifg=" bg-main " cterm=none ctermbg=2 ctermfg=" termcolbg "" "\n"
"hi StatusLineTermNC gui=none guibg=" bg-alt " guifg=" green " cterm=none ctermbg=" termcolbgalt " ctermfg=2" "\n"
"\n"
"hi VertSplit gui=none cterm=none" "\n"
"hi TabLine gui=none guibg=" bg-alt " guifg=" fg-alt " cterm=none ctermbg=" termcolbgalt " ctermfg=" termcolfgalt "" "\n"
"hi TabLineSel gui=none guibg=" cyan " guifg=" bg-main " cterm=none ctermbg=6 ctermfg=" termcolbg "" "\n"
"hi TabLineFill gui=none cterm=none" "\n"
"\n"
"hi Comment gui=italic guifg=" fg-alt " cterm=none ctermfg=" termcolfgalt "" "\n"
"hi Todo guibg=" bg-dim " guifg=" yellow-alt " ctermbg=" termcolbg " ctermfg=11" "\n"
"\n"
"hi Warning gui=none guibg=" yellow " guifg=" bg-main " cterm=none ctermbg=3 ctermfg=" termcolbg "" "\n"
"hi WarningMsg gui=none guibg=" yellow " guifg=" bg-main " cterm=none ctermbg=3 ctermfg=" termcolbg "" "\n"
"hi Error gui=none guibg=" red " guifg=" bg-main " cterm=none ctermbg=1 ctermfg=" termcolbg "" "\n"
"hi ErrorMsg gui=none guibg=" red " guifg=" bg-main " cterm=none ctermbg=1 ctermfg=" termcolbg "" "\n"
"\n"
"hi MatchParen gui=underline,bold guibg=" bg-alt " guifg=" fg-alt " cterm=underline,bold ctermbg=" termcolbgalt " ctermfg=" termcolfgalt "" "\n"
"\n"
"hi ToolbarLine guibg=" fg-alt " guifg=" bg-main " term=none ctermbg=" termcolfgalt " ctermfg=" termcolbg "" "\n"
"hi ToolbarButton gui=bold guibg=" fg-alt " guifg=" bg-main " term=none cterm=bold ctermbg=" termcolfgalt " ctermfg=" termcolbg "" "\n"
"\n"
"hi WildMenu guibg=" bg-main " guifg=" fg-main " term=standout ctermbg=" termcolbg " ctermfg=" termcolfg "" "\n"
"\n"
"hi Terminal guibg=" bg-main " guifg=" fg-main " term=none ctermbg=" termcolbg " ctermfg=" termcolfg "" "\n"
"\n"
"\" Constructs" "\n"
"\" ----------" "\n"
"hi Constant guifg=" blue-alt-other " ctermfg=4" "\n"
"hi Boolean guifg=" green " ctermfg=2" "\n"
"hi Number guifg=" blue-alt-other " ctermfg=4" "\n"
"hi Float guifg=" blue-alt-other " ctermfg=4" "\n"
"hi String guifg=" blue-alt " ctermfg=12" "\n"
"\n"
"hi Function guifg=" magenta " ctermfg=5" "\n"
"hi Identifier gui=none guifg=" cyan " cterm=none ctermfg=6" "\n"
"hi Label guifg=" magenta " ctermfg=5" "\n"
"hi Tag guifg=" magenta " ctermfg=5" "\n"
"hi Keyword gui=none guifg=" magenta-alt " cterm=none ctermfg=5" "\n"
"\n"
"hi Character gui=bold guifg=" cyan-alt-other " cterm=bold ctermfg=14" "\n"
"\n"
"hi Type gui=none,bold guifg=" cyan " term=none cterm=none,bold ctermfg=6" "\n"
"hi StorageClass guifg=" cyan " ctermfg=6" "\n"
"hi Structure guifg=" cyan " ctermfg=6" "\n"
"hi Typedef gui=bold guifg=" cyan-alt-other " cterm=bold ctermfg=14" "\n"
"\n"
"hi Conditional gui=none guifg=" magenta-alt-other " cterm=none ctermfg=13" "\n"
"hi Statement gui=none guifg=" magenta-alt-other " cterm=none ctermfg=13" "\n"
"hi Repeat gui=bold guifg=" magenta-alt " cterm=bold ctermfg=5" "\n"
"hi Operator gui=none guifg=" fg-main " cterm=none ctermfg=" termcolfg "\n"
"hi Exception gui=bold guifg=" yellow " cterm=bold ctermfg=2" "\n"
"\n"
"hi Preproc gui=none guifg=" red-alt " term=none cterm=none ctermfg=9" "\n"
"hi PreCondit gui=bold guifg=" red-alt " cterm=bold ctermfg=9" "\n"
"hi Macro gui=bold guifg=" magenta-alt " cterm=bold ctermfg=5" "\n"
"hi Include guifg=" red-alt " ctermfg=9" "\n"
"hi Define guifg=" red-alt " ctermfg=9" "\n"
"\n"
"hi Title gui=bold guibg=" bg-main " guifg=" cyan " cterm=bold ctermbg=" termcolbg " ctermfg=6" "\n"
"\n"
"hi Delimiter gui=none guifg=" fg-main " cterm=none ctermfg=" termcolfg "\n"
"hi SpecialComment gui=bold guifg=" yellow-alt-other " cterm=bold ctermfg=3" "\n"
"\n"
"hi Debug guifg=" magenta-alt " ctermfg=13" "\n"
"\n"
"\" Other" "\n"
"\" -----" "\n"
"hi LineNr guibg=" bg-alt " guifg=" fg-alt " term=none ctermbg=" termcolbgalt " ctermfg=" termcolfgalt "" "\n"
"hi Cursor guibg=" fg-main " guifg=" bg-main " ctermbg=" termcolfg " ctermfg=" termcolbg "" "\n"
"hi CursorLine gui=none guibg=" bg-alt " term=none cterm=none ctermbg=" termcolbgalt "" "\n"
"hi CursorColumn gui=none guibg=" bg-alt " term=none cterm=none ctermbg=" termcolbgalt "" "\n"
"hi CursorLineNr gui=bold guibg=" fg-alt " guifg=" bg-main " cterm=bold ctermbg=" termcolfgalt " ctermfg=" termcolbg "" "\n"
"hi ColorColumn guibg=" bg-alt " guifg=" fg-main " term=none ctermbg=" termcolbgalt " ctermfg=" termcolfg "" "\n"
"hi SignColumn guibg=" bg-alt " guifg=" fg-alt " term=none ctermbg=" termcolbgalt " ctermfg=" termcolfgalt "" "\n"
"\n"
"hi Folded guibg=" bg-alt " guifg=" fg-alt " ctermbg=" termcolbgalt " ctermfg=" termcolfgalt "" "\n"
"hi FoldColumn guibg=" bg-alt " guifg=" fg-alt " ctermbg=" termcolbgalt " ctermfg=" termcolfgalt "" "\n"
"\n"
"hi Special gui=none guifg=" green-alt-other " term=none cterm=none ctermfg=2" "\n"
"hi SpecialKey gui=none guibg=" bg-alt " guifg=" fg-alt " cterm=none ctermbg=" termcolbgalt " ctermfg=" termcolfgalt "" "\n"
"hi SpecialChar gui=bold guifg=" yellow-alt " cterm=bold ctermfg=11" "\n"
"hi NonText gui=none guibg=" bg-alt " guifg=" fg-alt " cterm=none ctermbg=" termcolbgalt " ctermfg=" termcolfgalt "" "\n"
"hi EndOfBuffer gui=bold guifg=" fg-alt " cterm=bold ctermfg=" termcolfgalt "" "\n"
"\n"
"hi Directory gui=none guifg=" green " term=none cterm=none ctermfg=2" "\n"
"hi Question gui=bold guifg=" yellow-alt " cterm=bold ctermfg=11" "\n"
"hi MoreMsg guifg=" green-alt " ctermfg=10" "\n"
"hi ModeMsg gui=bold guifg=" green " cterm=bold ctermfg=2" "\n"
"\n"
"hi VimOption guifg=" cyan-alt " ctermfg=6" "\n"
"hi VimGroup guifg=" cyan-alt " ctermfg=5" "\n"
"\n"
"hi Underlined gui=underline guifg=" fg-main " cterm=underline ctermfg=" termcolfg "" "\n"
"hi Ignore guibg=" bg-alt " guifg=" fg-alt " ctermbg=" termcolbgalt " ctermfg=" termcolfgalt "" "\n"
"hi Conceal guibg=" fg-alt " guifg=" bg-alt " ctermbg=" termcolfgalt " ctermfg=" termcolbgalt "" "\n"
"\n"
"hi SpellBad guibg=" red " guifg=" bg-main " ctermbg=1 ctermfg=" termcolbg "" "\n"
"hi SpellCap guibg=" yellow " guifg=" bg-main " ctermbg=3 ctermfg=" termcolbg "" "\n"
"hi SpellRare guibg=" magenta-alt-other " guifg=" bg-main " ctermbg=13 ctermfg=" termcolbg "" "\n"
"hi SpellLocal guibg=" cyan-alt-other " guifg=" bg-main " ctermbg=14 ctermfg=" termcolbg "" "\n"
"\n"
"hi Pmenu gui=italic guibg=" bg-alt " guifg=" fg-main " cterm=none ctermbg=" termcolbgalt " ctermfg=" termcolfg "" "\n"
"hi PmenuSel gui=none,bold guibg=" fg-alt " guifg=" bg-main " cterm=none,bold ctermbg=" termcolfgalt " ctermfg=" termcolbg "" "\n"
"hi PmenuSbar guibg=" bg-alt " ctermbg=" termcolbgalt "" "\n"
"hi PmenuThumb guibg=" fg-alt " ctermbg=" termcolfgalt "" "\n"
"\n"
"\" Terminal" "\n"
"\" --------" "\n"
"\n"
"if exists('*term_setansicolors')" "\n"
"	let g:terminal_ansi_colors = [" "\n"
"				\\ '#000000'," "\n"
"				\\ '" red "'," "\n"
"				\\ '" green "'," "\n"
"				\\ '" yellow "'," "\n"
"				\\ '" blue "'," "\n"
"				\\ '" magenta "'," "\n"
"				\\ '" cyan "'," "\n"
"				\\ '#bfbfbf'," "\n"
"				\\ '#595959'," "\n"
"				\\ '" red-alt "'," "\n"
"				\\ '" green-alt "'," "\n"
"				\\ '" yellow-alt "'," "\n"
"				\\ '" blue-alt "'," "\n"
"				\\ '" magenta-alt-other "'," "\n"
"				\\ '" cyan-alt-other "'," "\n"
"				\\ '#ffffff'" "\n"
"				\\ ]" "\n"
"endif" "\n"
"if has('nvim')" "\n"
"	let g:terminal_color_0 = '#000000'" "\n"
"	let g:terminal_color_1 = '" red "'" "\n"
"	let g:terminal_color_2 = '" green "'" "\n"
"	let g:terminal_color_3 = '" yellow "'" "\n"
"	let g:terminal_color_4 = '" blue "'" "\n"
"	let g:terminal_color_5 = '" magenta "'" "\n"
"	let g:terminal_color_6 = '" cyan "'" "\n"
"	let g:terminal_color_7 = '#bfbfbf'" "\n"
"	let g:terminal_color_8 = '#595959'" "\n"
"	let g:terminal_color_9 = '" red-alt "'" "\n"
"	let g:terminal_color_10 = '" green-alt "'" "\n"
"	let g:terminal_color_11 = '" yellow-alt "'" "\n"
"	let g:terminal_color_12 = '" blue-alt "'" "\n"
"	let g:terminal_color_13 = '" magenta-alt-other "'" "\n"
"	let g:terminal_color_14 = '" cyan-alt-other "'" "\n"
"	let g:terminal_color_15 = '#ffffff'" "\n"
"endif" "\n"
"\n"
"\" Diffs" "\n"
"\" -----" "\n"
"hi DiffAdd gui=bold guibg=" green " guifg=" bg-main " cterm=bold ctermbg=10 ctermfg=" termcolbg "" "\n"
"hi DiffDelete gui=none guibg=" red " guifg=" bg-main " cterm=none ctermbg=9 ctermfg=" termcolbg "" "\n"
"hi DiffChange gui=bold guibg=" bg-alt " guifg=" fg-alt " cterm=bold ctermbg=" termcolbgalt " ctermfg=" termcolfgalt "" "\n"
"hi DiffText gui=bold guibg=" bg-alt " guifg=" red-alt " cterm=bold ctermbg=" termcolbgalt " ctermfg=1" "\n"
"\n"
"hi diffAdded guifg=" green " ctermfg=2" "\n"
"hi diffRemoved guifg=" red " ctermfg=1" "\n"
"hi diffNewFile gui=none guifg=" blue " ctermfg=4" "\n"
"hi diffFile gui=none guifg=" yellow " cterm=none ctermfg=3" "\n"
"\n"
"hi GitGutterAdd guibg=" bg-alt " guifg=" green " ctermbg=" termcolbgalt " ctermfg=2" "\n"
"hi GitGutterChange gui=bold guibg=" bg-alt " guifg=" fg-alt " cterm=bold ctermbg=" termcolbgalt " ctermfg=" termcolfgalt "" "\n"
"hi GitGutterDelete guibg=" bg-alt " guifg=" red " ctermbg=" termcolbgalt " ctermfg=1" "\n"
"hi GitGutterChangeDelete gui=bold guibg=" bg-alt " guifg=" red " cterm=bold ctermbg=" termcolbgalt " ctermfg=1" "\n"
"\n"
"\" Neomake" "\n"
"\" -------" "\n"
"hi NeomakeError gui=none guibg=" red " guifg=" bg-main " cterm=none ctermbg=1 ctermfg=" termcolbg "" "\n"
"hi NeomakeInfo gui=none guibg=" cyan " guifg=" bg-main " cterm=none ctermbg=6 ctermfg=" termcolbg "" "\n"
"hi NeomakeWarning gui=none guibg=" yellow " guifg=" bg-main " cterm=none ctermbg=3 ctermfg=" termcolbg "" "\n"
"hi NeomakeMessage gui=none guibg=" yellow-alt " guifg=" bg-main " cterm=none ctermbg=11 ctermfg=" termcolbg "" "\n"
"\n"
"hi NeomakeVirtualtextInfoDefault guifg=" cyan-alt-other " ctermfg=14" "\n"
"hi NeomakeVirtualtextMessageDefault guifg=" yellow-alt " ctermfg=11" "\n"
"hi NeomakeVirtualtextWarningDefault guifg=" yellow " ctermfg=3" "\n"
"hi NeomakeVirtualtextErrorDefault guifg=" red " ctermfg=1" "\n"
"\n"
"hi NeomakeStatusGood gui=none guibg=" green " guifg=" bg-main " cterm=none ctermbg=2 ctermfg=" termcolbg "" "\n"
"hi NeomakeStatusGoodNC gui=none guibg=" bg-alt " guifg=" green " cterm=none ctermbg=" termcolbgalt " ctermfg=2" "\n"
"\n"
"hi NeomakeStatColorDefault gui=none guibg=" blue " guifg=" bg-main " cterm=none ctermbg=4 ctermfg=" termcolbg "" "\n"
"hi NeomakeStatColorTypeE gui=none guibg=" red " guifg=" bg-main " cterm=none ctermbg=1 ctermfg=" termcolbg "" "\n"
"hi NeomakeStatColorTypeW gui=none guibg=" yellow " guifg=" bg-main " cterm=none ctermbg=3 ctermfg=" termcolbg "" "\n"
"\n"
"\" Markdown" "\n"
"\" --------" "\n"
"hi MarkdownRule gui=bold guibg=" bg-alt " guifg=" green-alt " cterm=bold ctermbg=" termcolbgalt " ctermfg=10" "\n"
"\n"
"hi MarkdownHeading gui=bold guifg=" fg-main " cterm=bold ctermfg=" termcolfg "" "\n"
"hi default link MarkdownH1 MarkdownHeading" "\n"
"hi default link MarkdownH2 MarkdownHeading" "\n"
"hi default link MarkdownH3 MarkdownHeading" "\n"
"hi default link MarkdownH4 MarkdownHeading" "\n"
"hi default link MarkdownH5 MarkdownHeading" "\n"
"hi default link MarkdownH6 MarkdownHeading" "\n"
"hi default link MarkdownHeadingDelimiter MarkdownHeading" "\n"
"hi default link MarkdownHeadingRule MarkdownHeading" "\n"
"\n"
"hi MarkdownBold gui=bold guifg=" red-alt " cterm=bold ctermfg=9" "\n"
"hi default link MarkdownBoldDelimiter MarkdownBold" "\n"
"\n"
"hi MarkdownItalic gui=italic guifg=" yellow " cterm=none ctermfg=3" "\n"
"hi default link MarkdownItalicDelimiter MarkdownItalic" "\n"
"\n"
"hi MarkdownUrl gui=underline guifg=" blue " cterm=underline ctermfg=4" "\n"
"hi MarkdownLinkText gui=none guifg=" blue-alt " cterm=none ctermfg=12" "\n"
"hi MarkdownLinkDelimiter gui=bold guifg=" fg-main " cterm=bold ctermfg=" termcolfg "" "\n"
"hi default link MarkdownLinkTextDelimiter MarkdownLinkDelimiter" "\n"
"\n"
"hi MarkdownCode guifg=" magenta " ctermfg=5" "\n"
"hi default link MarkdownCodeDelimiter MarkdownCode" "\n"
"\n"
"hi MarkdownCodeBlock guifg=" fg-main " ctermfg=" termcolfg "" "\n"
"\n"
"hi MarkdownListMarker gui=none guifg=" green " cterm=none ctermfg=2" "\n"
"hi default link MarkdownOrderedListMarker MarkdownListMarker" "\n"
"\n"
"\" Linting" "\n"
"\" -------" "\n"
"hi YcmErrorSection gui=undercurl guisp=" fg-lang-error " cterm=underline" "\n"
"hi YcmWarningSection gui=undercurl guisp=" fg-lang-warning " cterm=underline" "\n"
"hi SyntasticError gui=undercurl guisp=" fg-lang-error " cterm=underline" "\n"
"hi SyntasticWarning gui=undercurl guisp=" fg-lang-warning " cterm=underline" "\n"
"hi SyntasticErrorSing guifg=" bg-main " guibg=" fg-lang-error " ctermfg=" termcolbg " ctermbg=1" "\n"
"hi SyntasticWarningSign guifg=" bg-main " guibg=" fg-lang-warning " ctermfg=" termcolbg " ctermbg=3" "\n")))))

;;;; iterm template

(defun modus-themes-exporter--iterm2-color-component (color component)
  "Generate a string value for the COMPONENT of the specified COLOR.

The COLOR is a string in a form #RRGGBB.  The component is one of: 'red, 'green, or 'blue."
  (number-to-string
   (/ (string-to-number (pcase component
                          ('red (substring color 1 3))
                          ('green (substring color 3 5))
                          ('blue (substring color 5 7)))
                        16)
      255.0)))

(defun modus-themes-exporter--iterm2-color-name (name)
  "Generate a DOM key entry for the specified NAME."
  `(key nil ,name))

(defun modus-themes-exporter--iterm2-color-dict (color &optional alpha)
  "Generate a DOM dict entry entry for the specified COLOR and optional ALPHA.
The COLOR is a string in a form of #RRGGBB.  The ALPHA is a number."
  `(dict nil
         (key nil "Alpha Component")
         (real nil ,(number-to-string (or alpha 1)))
         (key nil "Blue Component")
         (real nil ,(modus-themes-exporter--iterm2-color-component color 'blue))
         (key nil "Color Space")
         (string nil "sRGB")
         (key nil "Green Component")
         (real nil ,(modus-themes-exporter--iterm2-color-component color 'green))
         (key nil "Red Component")
         (real nil ,(modus-themes-exporter--iterm2-color-component color 'red))))

(defun modus-themes-exporter--iterm2-dict (colors-defs)
  "Generate a string representation of COLORS-DEFS alist for iTerm2 colors.

Each item in COLORS-DEFS is in a form of (NAME . VALUE), where NAME is one of
the iTerm 2 color names and VALUE either is a color string in a form of #RRGGBB,
or is a list of color string in a from of #RRGGBB and an alpha value."
  (with-temp-buffer
    (dom-print
     `(dict nil
            ,@(apply 'cl-concatenate
                     'list
                     (mapcar (lambda (color-name)
                               (list (modus-themes-exporter--iterm2-color-name (car color-name))
                                     (apply 'modus-themes-exporter--iterm2-color-dict (pcase (cdr color-name)
                                                                                        ((and (pred listp) args) args)
                                                                                        (arg (list arg))))))
                             colors-defs)))
     t t)
    (buffer-string)))

(defun modus-themes-exporter-iterm2 ()
  "Template for iTerm2."
  (modus-themes-with-colors
    (let ((theme-name (format "%s" (modus-themes-exporter--current-theme))))
      (with-temp-buffer
        (concat
         "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
         "<!DOCTYPE plist PUBLIC \"-//Apple//DTD PLIST 1.0//EN\" \"http://www.apple.com/DTDs/PropertyList-1.0.dtd\">\n"
         "<!-- Theme: " theme-name "-->\n"
         "<!-- Description: iTerm2 port of " theme-name " (Modus themes for Emacs)" "-->\n"
         "<!-- Author: Przemek Kryger" "-->\n"
         "<plist version=\"1.0\">\n"
         (modus-themes-exporter--iterm2-dict
          `(("Ansi 0 Color" . "#000000")
            ("Ansi 1 Color" . ,red)
            ("Ansi 2 Color" . ,green)
            ("Ansi 3 Color" . ,yellow)
            ("Ansi 4 Color" . ,blue)
            ("Ansi 5 Color" . ,magenta)
            ("Ansi 6 Color" . ,cyan)
            ("Ansi 7 Color" . "#bfbfbf")
            ("Ansi 8 Color" . "#595959")
            ("Ansi 9 Color" . ,red-alt)
            ("Ansi 10 Color" . ,green-alt)
            ("Ansi 11 Color" . ,yellow-alt)
            ("Ansi 12 Color" . ,blue-alt)
            ("Ansi 13 Color" . ,magenta-alt-other)
            ("Ansi 14 Color" . ,cyan-alt-other)
            ("Ansi 15 Color" . "#ffffff")
            ("Background Color" . ,bg-main)
            ("Badge Color" . (,red-alt 0.75))
            ("Bold Color" . ,fg-main)
            ("Cursor Color" . ,fg-main)
            ("Cursor Guide Color" . (,bg-hl-line 0.8))
            ("Cursor Text Color" . ,bg-main)
            ("Foreground Color" . ,fg-main)
            ("Link Color" . ,blue-alt)
            ("Selected Text Color" . ,fg-main)
            ("Selection Color" . ,bg-region)))
         "\n"
         "</plist>" "\n")))))

;;;; Export command

(defvar modus-themes-exporter-template-hist '()
  "History of inputs for templates.")

(defun modus-themes-exporter--export-prompt (apps)
  "Helper for `modus-themes-exporter-export' to select among APPS."
  (completing-read "Select template: " apps nil t nil 'modus-themes-exporter-template-hist))

;;;###autoload
(defun modus-themes-exporter-export (template &optional file no-visit)
  "Export current Modus theme using TEMPLATE.

When called interactively, TEMPLATE is chosen from a list of candidates
using completion.  Else it must be a string that corresponds to the car
of a cons cell in `modus-themes-exporter-templates-alist'.

The output is stored in the kill ring.

When called from Lisp with optional FILE as a path to a regular file,
write there directly.  When called interactively with the universal
prefix argument (\\[universal-argument]), prompt for FILE instead:
supplying a non-existent path will create that file outright.  Once the
output has been written to the file, prompt to visit it.

With optional NO-VISIT, either as a non-nil symbol in Lisp or a
double prefix argument interactively, do not prompt to visit the
file."
  (interactive
   (list
    (modus-themes-exporter--export-prompt
     (mapcar #'car modus-themes-exporter-templates-alist))
    current-prefix-arg))
  (let* ((modus-themes '(modus-operandi modus-vivendi))
         (current-theme (modus-themes-exporter--current-theme))
         (fn (funcall (cdr (assoc template modus-themes-exporter-templates-alist))))
         (path (when file
                 (if (stringp file)
                     file
                   (expand-file-name
                    (read-file-name "Select file: " nil default-directory))))))
    (unless (member current-theme modus-themes)
      (error "`%s' is not a member of %s" current-theme modus-themes))
    (cond
     ((when (and path (file-writable-p path) (file-regular-p path))
        (with-temp-buffer
          (insert (eval fn))
          (write-region (point-min) (point-max) path))
        (if (or no-visit (prefix-numeric-value '(16)))
            (message "Wrote %s template to %s" template path)
          (when (yes-or-no-p (format "Wrote to file; visit new %s?" path))
            (find-file path)))))
     ((when (and path
                 (not (file-regular-p path)))
        (when (yes-or-no-p "Not a valid FILE; print output at point instead?")
          (insert (eval fn)))))
     (t
      (kill-new fn)
      (message "Saved to kill-ring port of %s for %s" current-theme template)))))

(provide 'modus-themes-exporter)
;;; modus-themes-exporter.el ends here
