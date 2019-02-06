" Name: Tempus Autumn
" Author: Protesilaos Stavrou (https://protesilaos.com)
" Description: Dark theme with a palette inspired by earthly colours (WCAG AA compliant)

set background=dark
hi clear
if exists("syntax_on")
  syntax reset
endif
let g:colors_name = "tempus_autumn"

" General
" -----------------

" NOTE the ctermbg=none is for terminals with transparency
hi Normal guibg=#2b1d1a guifg=#a49ba0 ctermbg=none ctermfg=15
hi Visual guibg=#a49ba0 guifg=#2b1d1a ctermbg=15 ctermfg=0
hi Search gui=underline,bold,italic guibg=#809900 guifg=#2b1d1a cterm=underline,bold,italic ctermbg=2 ctermfg=0
hi IncSearch gui=underline,bold,italic guibg=#919078 guifg=#2b1d1a term=none cterm=underline,bold,italic ctermbg=7 ctermfg=0

hi StatusLine gui=none,bold guibg=#a49ba0 guifg=#2b1d1a cterm=none,bold ctermbg=15 ctermfg=0
hi StatusLineNC gui=none guibg=#35260f guifg=#919078 cterm=none ctermbg=8 ctermfg=7
hi StatusLineTerm gui=none,bold guibg=#809900 guifg=#2b1d1a cterm=none,bold ctermbg=2 ctermfg=0
hi StatusLineTermNC gui=none guibg=#35260f guifg=#809900 cterm=none ctermbg=8 ctermfg=2

hi VertSplit gui=none cterm=none
hi TabLine gui=none guibg=#35260f guifg=#919078 cterm=none ctermbg=8 ctermfg=7
hi TabLineSel gui=none guibg=#a49ba0 guifg=#2b1d1a cterm=none ctermbg=15 ctermfg=0
hi TabLineFill gui=none cterm=none

hi Comment gui=italic guifg=#919078 cterm=italic ctermfg=7
hi Todo gui=bold guibg=#35260f guifg=#b28910 cterm=bold ctermbg=8 ctermfg=11

hi Warning gui=none guibg=#ad8b00 guifg=#2b1d1a cterm=none ctermbg=3 ctermfg=0
hi WarningMsg gui=none guibg=#ad8b00 guifg=#2b1d1a cterm=none ctermbg=3 ctermfg=0
hi Error gui=none guibg=#f85a10 guifg=#2b1d1a cterm=none ctermbg=1 ctermfg=0
hi ErrorMsg gui=none guibg=#f85a10 guifg=#2b1d1a cterm=none ctermbg=1 ctermfg=0

hi MatchParen gui=underline,bold guibg=#c874a3 guifg=#2b1d1a cterm=underline,bold ctermbg=13 ctermfg=0

hi ToolbarLine guibg=#919078 guifg=#2b1d1a term=none ctermbg=7 ctermfg=0
hi ToolbarButton gui=bold guibg=#919078 guifg=#2b1d1a term=none cterm=bold ctermbg=7 ctermfg=0

hi WildMenu guibg=#2b1d1a guifg=#a49ba0 term=standout ctermbg=0 ctermfg=15

hi Terminal guibg=#2b1d1a guifg=#a49ba0 term=none ctermbg=0 ctermfg=15

" Constructs
" -----------------
hi Constant guifg=#888db0 ctermfg=4
hi Number guifg=#888db0 ctermfg=4
hi Float guifg=#888db0 ctermfg=4
hi String guifg=#9385cd ctermfg=12

hi Function guifg=#cb758e ctermfg=5
hi Identifier guifg=#c874a3 term=none ctermfg=13
hi Label guifg=#cb758e ctermfg=5
hi Tag guifg=#cb758e ctermfg=5
hi Keyword gui=bold guifg=#c874a3 gui=bold ctermfg=13

hi Character gui=bold guifg=#529d80 cterm=bold ctermfg=14

hi Type gui=none,bold guifg=#709a65 term=none cterm=none,bold ctermfg=6
hi Boolean guifg=#709a65 ctermfg=6
hi StorageClass guifg=#709a65 ctermfg=6
hi Structure guifg=#709a65 ctermfg=6
hi Typedef gui=bold guifg=#529d80 cterm=bold ctermfg=14

hi Conditional gui=bold guifg=#809900 cterm=bold ctermfg=2
hi Statement gui=none guifg=#729c1a cterm=none ctermfg=10
hi Repeat gui=bold guifg=#729c1a cterm=bold ctermfg=10
hi Operator gui=bold guifg=#a49ba0 cterm=bold ctermfg=15
hi Exception gui=bold guifg=#f85a10 cterm=bold ctermfg=1

hi Preproc gui=none guifg=#ce7b1a term=none cterm=none ctermfg=9
hi PreCondit gui=bold guifg=#ce7b1a cterm=bold ctermfg=9
hi Macro gui=bold guifg=#ce7b1a cterm=bold ctermfg=9
hi Include guifg=#ce7b1a ctermfg=9
hi Define guifg=#ce7b1a ctermfg=9

hi Title gui=bold guibg=#2b1d1a guifg=#709a65 cterm=bold ctermbg=0 ctermfg=6

hi Special gui=bold guifg=#b28910 term=none cterm=bold ctermfg=11
hi SpecialKey guifg=#b28910 ctermfg=11
hi SpecialChar gui=bold guifg=#b28910 cterm=bold ctermfg=11

hi Delimeter gui=bold guifg=#cb758e cterm=bold ctermfg=5
hi Delimiter gui=bold guifg=#cb758e cterm=bold ctermfg=5
hi SpecialComment gui=bold guifg=#cb758e cterm=bold ctermfg=5

hi Debug guifg=#c874a3 ctermfg=13

" Other
" -----------------
hi LineNr guibg=#35260f guifg=#919078 term=none ctermbg=8 ctermfg=7
hi Cursor guibg=#a49ba0 guifg=#2b1d1a ctermbg=15 ctermfg=0
hi CursorLine gui=none guibg=NONE term=none cterm=none ctermbg=none
hi CursorColumn gui=none guibg=#35260f term=none cterm=none ctermbg=8
hi CursorLineNr gui=bold guibg=#919078 guifg=#2b1d1a cterm=bold ctermbg=7 ctermfg=0
hi ColorColumn guibg=#35260f guifg=#a49ba0 term=none ctermbg=8 ctermfg=15

hi Folded guibg=#35260f guifg=#919078 ctermbg=8 ctermfg=7
hi FoldColumn guibg=#35260f guifg=#919078 ctermbg=8 ctermfg=7

hi NonText gui=bold guibg=NONE guifg=#919078 cterm=bold ctermbg=none ctermfg=7

hi Directory gui=none guifg=#809900 term=none cterm=none ctermfg=2
hi Question gui=bold guifg=#b28910 cterm=bold ctermfg=11
hi MoreMsg guifg=#729c1a ctermfg=10
hi ModeMsg gui=bold guifg=#809900 cterm=bold ctermfg=2

hi VimOption guifg=#cb758e ctermfg=5
hi VimGroup guifg=#cb758e ctermfg=5

hi Underlined gui=underline,bold guifg=#a49ba0 cterm=underline,bold ctermfg=15
hi Ignore guibg=#35260f guifg=#919078 ctermbg=8 ctermfg=7
hi Conceal guibg=#919078 guifg=#35260f ctermbg=7 ctermfg=8

hi SpellBad guibg=#f85a10 guifg=#2b1d1a ctermbg=1 ctermfg=0
hi SpellCap guibg=#ad8b00 guifg=#2b1d1a ctermbg=3 ctermfg=0
hi SpellRare guibg=#c874a3 guifg=#2b1d1a ctermbg=13 ctermfg=0
hi SpellLocal guibg=#529d80 guifg=#2b1d1a ctermbg=14 ctermfg=0

hi Pmenu gui=italic guibg=#35260f guifg=#a49ba0 cterm=italic ctermbg=8 ctermfg=15
hi PmenuSel gui=none,bold guibg=#919078 guifg=#2b1d1a cterm=none,bold ctermbg=7 ctermfg=0
hi PmenuSbar guibg=#35260f ctermbg=8
hi PmenuThumb guibg=#919078 ctermbg=7

" Diffs
" -----------------
hi DiffAdd gui=bold guibg=#809900 guifg=#2b1d1a cterm=bold ctermbg=10 ctermfg=0
hi DiffDelete gui=none guibg=#f85a10 guifg=#2b1d1a cterm=none ctermbg=9 ctermfg=0
hi DiffChange gui=bold guibg=#35260f guifg=#919078 cterm=bold ctermbg=8 ctermfg=7
hi DiffText gui=bold guibg=#35260f guifg=#ce7b1a cterm=bold ctermbg=8 ctermfg=1

hi diffAdded guifg=#809900 ctermfg=2
hi diffRemoved guifg=#f85a10 ctermfg=1
hi diffNewFile gui=none guifg=#888db0 ctermfg=4
hi diffFile gui=none guifg=#ad8b00 cterm=none ctermfg=3
