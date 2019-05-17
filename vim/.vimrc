"
" ~/.vimrc
"

" Description
" -----------
"
" This is my Vim config file.  It should be distributed together with my
" '.vim' directory.  I try to keep the entire setup simple.  I use no
" plugins whatsoever, have no custom key bindings, while I do not
" leverage ANY of the multiplexing and session-management features of
" vim: that is the job of tmux, the window manager, or whatever
" dedicated tool.  For me, Vim is just a text editor.
"
" This is part of my dotfiles, which deploy a custom desktop session
" based on BSPWM: https://gitlab.com/protesilaos/dotfiles.
"
" The colour scheme is part of my Tempus Themes (designed for
" accessibility): https://protesilaos.com/tempus-themes.  Theme files
" are bundled with my dots (Tempus themes are fully integrated into my
" dotfiles and are applied almost everywhere).
"
" I run Vim exclusively in the console, but also install a graphical
" version as a fallback option.  On Debian 'buster':
"
"	sudo apt install vim vim-gtk3
"
" Last full review: 2019-05-09

" General settings
" ----------------
" Read documentation about each option by executing :h <option>

set nocompatible                  " do not preserve compatibility with Vi
set modifiable                    " buffer contents can be modified
set encoding=utf-8                " default character encoding
set autoread                      " detect when a file has been modified externally
set spelllang=en,el               " languages to check for spelling (english, greek)
set spellsuggest=10               " number of suggestions for correct spelling
set updatetime=500                " time of idleness is miliseconds before saving swapfile
set undolevels=1000               " how many undo levels to keep in memory
set showcmd                       " show command in last line of the screen
set nostartofline                 " keep cursor in the same column when moving between lines
set errorbells                    " ring the bell for errors
set visualbell                    " then use a flash instead of a beep sound
set confirm                       " ask for confirmation when quitting a file that has changes
set hidden                        " hide buffers
set autoindent                    " indent automatically (useful for formatoptions)
set noexpandtab                   " use tabs instead of spaces
set tabstop=4                     " tab character width
set shiftwidth=4                  " needs to be the same as tabstop
set smartcase                     " ignore case if the search contains majuscules
set hlsearch                      " highlight all matches of last search
set incsearch                     " enable incremental searching (get feedback as you type)
set backspace=indent,eol,start    " backspace key should delete indentation, line ends, characters
set whichwrap=s,b                 " which motion keys should jump to the above/below wrapped line
set textwidth=72                  " hard wrap at this column
set joinspaces                    " insert two spaces after puncutation marks when joining multiple lines into one
set wildmenu                      " enable tab completion with suggestions when executing commands
set wildmode=list:longest,full    " settings for how to complete matched strings
set nomodeline                    " vim reads the modeline to execute commands for the current file
set modelines=0                   " how many lines to check in the top/bottom of the file. 0=off

" defines how automatic formatting should be done (see :h fo-table)
set formatoptions-=t formatoptions-=o formatoptions+=crqjnl1
filetype plugin on                " load syntax options for different file types
filetype indent off               " do not load indent options for different file types

" invisible characters to display (with :set list)
set listchars=tab:›\ ,nbsp:␣,trail:•,precedes:«,extends:»
""""set listchars=tab:›\ ,space:·,nbsp:␣,trail:•,eol:¬,precedes:«,extends:»

" Helpers
" -------

" abbreviations (try not to use common words)
iab medate <c-r>=strftime('%Y-%m-%d')<cr>
iab meweb https://protesilaos.com/
iab megit https://gitlab.com/protesilaos
iab medot https://gitlab.com/protesilaos/dotfiles

" Change cursor shape in different modes.  Adapted from:
" http://vim.wikia.com/wiki/Change_cursor_shape_in_different_modes
"
" Cursor shape follows the DECSCUSR standard:
"
" 	default, 0, 1, or none = blinking block
" 	2 = steady block
" 	3 = blinking underscore
" 	4 = steady underscore
" 	5 = blinking bar (i-beam)
" 	6 = steady bar
if exists('$TMUX')
	let &t_SI = "\ePtmux;\e\e[6 q\e\\"
	let &t_SR = "\ePtmux;\e\e[4 q\e\\"
	let &t_EI = "\ePtmux;\e\e[2 q\e\\"
else
	let &t_SI = "\e[6 q"
	let &t_SR = "\e[4 q"
	let &t_EI = "\e[2 q"
endif

" Theme settings
" --------------

syntax enable                 " enable syntax highlighting
colorscheme tempus_classic    " use one of my Tempus themes

" DO NOT EDIT MANUALLY IF YOU USE MY DOTFILES.  Themes are updated
" programatically via my `own_script_update_environment_theme`
