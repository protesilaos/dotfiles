" 
" ~/.vimrc
"

" Description {{{
" ---------------
"
" This is my Vim config file.  I try to keep it simple and do things
" without heavily modifying the meaning of the commands.  I use no
" plugins whatsoever.  Additional functionality is provided by my
" operating system (GNU/Linux). You can read this as to why:
" https://protesilaos.com/codelog/2018-11-24-vim-no-plugins/
"
" This setup is part of my dotfiles, which deploy a custom working
" environment based on BSPWM.  The underlying OS is Debian Buster/Sid.
" See https://gitlab.com/protesilaos/dotfiles.
" 
" The syntax theme is part of my Tempus Themes project. See
" https://protesilaos.com/tempus-themes.  Theme files are bundled with
" my dotfiles (Tempus is used everywhere).
" 
" I run Vim exclusively in the console, but also install a graphical
" version as a fallback option.  On Debian, install with:
"
"	sudo apt install vim vim-gtk3
"
" Last full review: 2018-11-25
"
" }}}

" General settings {{{
" ====================
" Read documentation about each option by executing :h <option>

set nocompatible " do not preserve compatibility with Vi
set modifiable " buffer contents can be modified

" file settings
set encoding=utf-8 " default character encoding
set autoread " detect when a file has been modified externally
set spelllang=en,el " languages to check for spelling
set spellsuggest=10 " number of suggestions for correct spelling
set updatetime=500 " time of idleness is miliseconds before saving swapfile
set undolevels=1000 " how many undo levels to keep in memory
set showcmd " show command in last line of the screen

" modeline
" NOTE this can be a security risk or an inconvenience, so disable or
" adapt accordingly when working with untrusted files.
set modeline " vim reads this to execute commands for the current file
set modelines=2 " how many lines to check in the top/bottom of the file

" cursor
set nostartofline " keep cursor in the same column when moving between lines

" error feedback
set errorbells " ring the bell for errors
set visualbell " then use a flash instead of a beep sound

" white space
set backspace=indent,eol,start " backspace key should delete indentation, line ends, characters
set whichwrap=s,b " which motions keys should jump to the above/below wrapped line

" context menu
set wildmenu " enable tab completion with suggestions when executing commands
set wildmode=list:longest,full " settings for how to complete matched strings
set wildignore+=*.bmp,*.gif,*.ico,*.jpg,*.png,*.ico " ignore images
set wildignore+=*.pdf,*.psd " ignore documents
set wildignore+=*.swp,*.bak " ignore swap files
set wildignore+=node_modules/*,bower_components/*,vendor/*,.vendor/*,_site/*,.git/* " ignore development files

" buffers
set confirm " ask for confirmation when quitting a file that has changes
set hidden " hide buffers
set listchars=tab:›\ ,space:·,nbsp:␣,trail:•,eol:¬,precedes:«,extends:» " invisible characters to display (with :set list)

" tabs
set autoindent " indent automatically (useful for formatoptions)
set noexpandtab " use tabs instead of spaces
set tabstop=4 " tab character width
set shiftwidth=4 " needs to be the same as tabstop

" searches
set smartcase " ignore case if the search contains majuscules
set hlsearch " highlight all matches of last search
set incsearch " enable incremental searching (get feedback as you type)

" windows/splits
set winminwidth=0 " minimum width of new windows/splits
set winminheight=0 " minimum  height
set splitbelow splitright " put new window below and to the right of the current one

" formatting (see :h fo-table)
" NOTE some custom commands are about formatting, see the 'Commands'
" sub-section below in the key bindings section.
set textwidth=72 " hard wrap at this column
set joinspaces " insert two spaces after puncutation marks when joining multiple lines into one
set formatoptions-=t formatoptions-=o formatoptions+=crqjnl1 " defines how automatic formatting should be done
filetype plugin on " load syntax options for different file types
filetype indent off " do not load indent options for different file types

" }}}

" Helpers {{{
" ===========

" auto reload vimrc when editing it
autocmd! bufwritepost .vimrc source ~/.vimrc

" abbreviations
iab medate <c-r>=strftime('%Y-%m-%d')<cr>
iab metime <c-r>=strftime('%H:%M')<cr>
iab meweb https://protesilaos.com/
iab megithub https://github.com/protesilaos
iab medots https://gitlab.com/protesilaos/dotfiles
iab megitlab https://gitlab.com/protesilaos

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

" Identify the highlight group under the cursor.  Useful for debugging
" my Vim themes.
map <F10> :echo "hi<" . synIDattr(synID(line("."),col("."),1),"name") . '> trans<'
\ . synIDattr(synID(line("."),col("."),0),"name") . "> lo<"
\ . synIDattr(synIDtrans(synID(line("."),col("."),1)),"name") . ">"<CR>

" }}}

" Key bindings {{{
" ================

" [mode]map : define a new mapping recursively
" [mode]remap : redefine a default/existing mapping recursively
" [mode]noremap : define a new mapping non-recursively
"
" NOTE avoid ambiguous mappings to speed up execution.

let mapleader=","

" editor interface
" ----------------

nnoremap <silent> \ :silent nohlsearch<cr> " clear the search highlight

"  folding lines
"  When inside a fold, <Space> will toggle open/close state.  Otherwise it
"  moves the cursor to the right (default behaviour).  In visual mode,
"  <Space> will create a fold.  This is taken from the Vim Wikia page:
"  http://vim.wikia.com/wiki/Folding
nnoremap <silent> <Space> @=(foldlevel('.')?'za':"\<Space>")<CR>
vnoremap <Space> zf

" move lines in normal mode (note that s-j joins lines in insert mode).
nnoremap <silent> <s-j> :m +1<cr>
nnoremap <silent> <s-k> :m -2<cr>

" Commands
" --------
" for the meaning of the formatoptions, see :h fo-table

" Clear all formatting options
nmap <leader>fno :set textwidth=0 formatoptions=<Esc>

" Plain text formatting for my writings
nmap <leader>ftx :set textwidth=72 formatoptions=twan12<Esc>

" Set formatting options for comments in source code so that text is
" automatically wrapped at text width. For the full list of options see `:h
" fo-table`. Also note that the minus sign does not work
" consistently when aggregating options.
" nmap <leader>fc :set formatoptions-=t formatoptions-=o formatoptions+=ncrql<Esc>
nmap <leader>fc :set formatoptions-=t formatoptions-=o formatoptions+=crqjnl1<Esc>

" }}}

" Status line {{{
" ===============
" TODO this sections needs to be simplified

" protline: my simplistic statusline
" 
" based off of sources:
" https://www.reddit.com/r/vim/comments/6b7b08/my_custom_statusline/
" https://hackernoon.com/the-last-statusline-for-vim-a613048959b2

" Dictionary: take mode() input -> longer notation of current mode
" mode() is defined by Vim
let g:currentmode={
    \ 'n'  : 'Normal ',
    \ 'no' : 'N·Operator Pending ',
    \ 'v'  : 'Visual ',
    \ 'V'  : 'V·Line ',
    \ '^V' : 'V·Block ',
    \ 's'  : 'Select ',
    \ 'S'  : 'S·Line ',
    \ '^S' : 'S·Block ',
    \ 'i'  : 'Insert ',
    \ 'R'  : 'Replace ',
    \ 'Rv' : 'V·Replace ',
    \ 'c'  : 'Command ',
    \ 'cv' : 'Vim Ex ',
    \ 'ce' : 'Ex ',
    \ 'r'  : 'Prompt ',
    \ 'rm' : 'More ',
    \ 'r?' : 'Confirm ',
    \ '!'  : 'Shell ',
    \ 't'  : 'Terminal '}

function! ProtLineCurrentMode() abort
    let l:modecurrent = mode()
    " use get() -> fails safely, since ^V doesn't seem to register
    " 3rd arg is used when return of mode() == 0, which is case with ^V
    " thus, ^V fails -> returns 0 -> replaced with 'V Block'
    let l:modelist = toupper(get(g:currentmode, l:modecurrent, 'V·Block '))
    let l:current_status_mode = l:modelist
    return l:current_status_mode
endfunction

function! ProtLinePasteMode()
    let paste_status = &paste
    if paste_status == 1
        return "paste"
    else
        return ""
    endif
endfunction

function! ProtLineActiveStatus()
    let statusline=""
    let statusline.="%#StatusLine#"
    let statusline.="\ %{ProtLineCurrentMode()}\%-6{ProtLinePasteMode()}"
    let statusline.="\%<"
    let statusline.="%#LineNr#"
    let statusline.="\ %F%( %m%)"
    let statusline.="\ %(\%r %h% %w%)"
    let statusline.="%=" 
    let statusline.=" %{&tw} %{&fo} "
    let statusline.="%#StatusLine#"
    let statusline.="\ %c-%l/%L\ "
    return statusline
endfunction

function! ProtLineActiveStatusInsertMode()
    let statusline=""
    let statusline.="%#StatusLineTerm#"
    let statusline.="\ %{ProtLineCurrentMode()}\%-6{ProtLinePasteMode()}"
    let statusline.="\%<"
    let statusline.="%#LineNr#"
    let statusline.="\ %F%( %m%)"
    let statusline.="\ %r"
    let statusline.="%=" 
    let statusline.=" %{&tw} %{&fo} "
    let statusline.="%#StatusLineTerm#"
    let statusline.="\ %c-%l/%L\ "
    return statusline
endfunction

function! ProtLineInactiveStatus()
    let statusline=""
    let statusline.="%#LineNr#"
    let statusline.="\%<"
    let statusline.="\ %F"
    let statusline.="\ %r"
    let statusline.="%=" 
    let statusline.="\ %c-%l/%L\ "
    return statusline
endfunction

set laststatus=2 " always draw a status bar
set noshowmode " do not show the mode at the last line (we have the status bar)
set statusline=%!ProtLineActiveStatus()

augroup status
    autocmd!
    autocmd WinEnter * setlocal statusline=%!ProtLineActiveStatus()
    autocmd WinLeave * setlocal statusline=%!ProtLineInactiveStatus()
    autocmd InsertEnter * setlocal statusline=%!ProtLineActiveStatusInsertMode()
    autocmd InsertLeave * setlocal statusline=%!ProtLineActiveStatus()
augroup END

" }}}

" Theme settings {{{
" ==================

set t_Co=256 " number of colours
syntax enable " enable syntax highlighting
colorscheme tempus_classic " use one of my Tempus thems (these are bundled with my dotfiles) - also as a plugin https://gitlab.com/protesilaos/tempus-themes-vim
" DO NOT EDIT MANUALLY.  Themes are updated programatically via my
" `own_script_update_environment_theme`

" }}}

" vi:foldmethod=marker
