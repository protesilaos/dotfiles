" Name: Tempus Classic
" Author: Protesilaos Stavrou (https://protesilaos.com)
" Description: Dark theme with warm hues (WCAG AA compliant)

set background=dark
hi clear
if exists("syntax_on")
  syntax reset
endif
let g:colors_name = "tempus_classic"

" General
" -----------------

" NOTE the ctermbg=none is for terminals with transparency
hi Normal guibg=#232323 guifg=#aeadaf ctermbg=none ctermfg=15
hi Visual guibg=#aeadaf guifg=#232323 ctermbg=15 ctermfg=0
hi Search gui=underline,bold,italic guibg=#8c9e3d guifg=#232323 cterm=underline,bold,italic ctermbg=2 ctermfg=0
hi IncSearch gui=underline,bold,italic guibg=#949d9f guifg=#232323 term=none cterm=underline,bold,italic ctermbg=7 ctermfg=0

hi StatusLine gui=none,bold guibg=#aeadaf guifg=#232323 cterm=none,bold ctermbg=15 ctermfg=0
hi StatusLineNC gui=none guibg=#312e30 guifg=#949d9f cterm=none ctermbg=8 ctermfg=7
hi StatusLineTerm gui=none,bold guibg=#8c9e3d guifg=#232323 cterm=none,bold ctermbg=2 ctermfg=0
hi StatusLineTermNC gui=none guibg=#312e30 guifg=#8c9e3d cterm=none ctermbg=8 ctermfg=2

hi VertSplit gui=none cterm=none
hi TabLine gui=none guibg=#312e30 guifg=#949d9f cterm=none ctermbg=8 ctermfg=7
hi TabLineSel gui=none guibg=#aeadaf guifg=#232323 cterm=none ctermbg=15 ctermfg=0
hi TabLineFill gui=none cterm=none

hi Comment gui=italic guifg=#949d9f cterm=italic ctermfg=7
hi Todo gui=bold guibg=#312e30 guifg=#a8a030 cterm=bold ctermbg=8 ctermfg=11

hi Warning gui=none guibg=#b1942b guifg=#232323 cterm=none ctermbg=3 ctermfg=0
hi WarningMsg gui=none guibg=#b1942b guifg=#232323 cterm=none ctermbg=3 ctermfg=0
hi Error gui=none guibg=#d2813d guifg=#232323 cterm=none ctermbg=1 ctermfg=0
hi ErrorMsg gui=none guibg=#d2813d guifg=#232323 cterm=none ctermbg=1 ctermfg=0

hi MatchParen gui=underline,bold guibg=#d58888 guifg=#232323 cterm=underline,bold ctermbg=13 ctermfg=0

hi ToolbarLine guibg=#949d9f guifg=#232323 term=none ctermbg=7 ctermfg=0
hi ToolbarButton gui=bold guibg=#949d9f guifg=#232323 term=none cterm=bold ctermbg=7 ctermfg=0

hi WildMenu guibg=#232323 guifg=#aeadaf term=standout ctermbg=0 ctermfg=15

hi Terminal guibg=#232323 guifg=#aeadaf term=none ctermbg=0 ctermfg=15

" Constructs
" -----------------
hi Constant guifg=#6e9cb0 ctermfg=4
hi Number guifg=#6e9cb0 ctermfg=4
hi Float guifg=#6e9cb0 ctermfg=4
hi String guifg=#8e9cc0 ctermfg=12

hi Function guifg=#b58d88 ctermfg=5
hi Identifier guifg=#d58888 term=none ctermfg=13
hi Label guifg=#b58d88 ctermfg=5
hi Tag guifg=#b58d88 ctermfg=5
hi Keyword gui=bold guifg=#d58888 gui=bold ctermfg=13

hi Character gui=bold guifg=#7aa880 cterm=bold ctermfg=14

hi Type gui=none,bold guifg=#6da280 term=none cterm=none,bold ctermfg=6
hi Boolean guifg=#6da280 ctermfg=6
hi StorageClass guifg=#6da280 ctermfg=6
hi Structure guifg=#6da280 ctermfg=6
hi Typedef gui=bold guifg=#7aa880 cterm=bold ctermfg=14

hi Conditional gui=bold guifg=#8c9e3d cterm=bold ctermfg=2
hi Statement gui=none guifg=#96a42d cterm=none ctermfg=10
hi Repeat gui=bold guifg=#96a42d cterm=bold ctermfg=10
hi Operator gui=bold guifg=#aeadaf cterm=bold ctermfg=15
hi Exception gui=bold guifg=#d2813d cterm=bold ctermfg=1

hi Preproc gui=none guifg=#d0913d term=none cterm=none ctermfg=9
hi PreCondit gui=bold guifg=#d0913d cterm=bold ctermfg=9
hi Macro gui=bold guifg=#d0913d cterm=bold ctermfg=9
hi Include guifg=#d0913d ctermfg=9
hi Define guifg=#d0913d ctermfg=9

hi Title gui=bold guibg=#232323 guifg=#6da280 cterm=bold ctermbg=0 ctermfg=6

hi Special gui=bold guifg=#a8a030 term=none cterm=bold ctermfg=11
hi SpecialKey guifg=#a8a030 ctermfg=11
hi SpecialChar gui=bold guifg=#a8a030 cterm=bold ctermfg=11

hi Delimeter gui=bold guifg=#b58d88 cterm=bold ctermfg=5
hi Delimiter gui=bold guifg=#b58d88 cterm=bold ctermfg=5
hi SpecialComment gui=bold guifg=#b58d88 cterm=bold ctermfg=5

hi Debug guifg=#d58888 ctermfg=13

" Other
" -----------------
hi LineNr guibg=#312e30 guifg=#949d9f term=none ctermbg=8 ctermfg=7
hi Cursor guibg=#aeadaf guifg=#232323 ctermbg=15 ctermfg=0
hi CursorLine gui=none guibg=NONE term=none cterm=none ctermbg=none
hi CursorColumn gui=none guibg=#312e30 term=none cterm=none ctermbg=8
hi CursorLineNr gui=bold guibg=#949d9f guifg=#232323 cterm=bold ctermbg=7 ctermfg=0
hi ColorColumn guibg=#312e30 guifg=#aeadaf term=none ctermbg=8 ctermfg=15

hi Folded guibg=#312e30 guifg=#949d9f ctermbg=8 ctermfg=7
hi FoldColumn guibg=#312e30 guifg=#949d9f ctermbg=8 ctermfg=7

hi NonText gui=bold guibg=NONE guifg=#949d9f cterm=bold ctermbg=none ctermfg=7

hi Directory gui=none guifg=#8c9e3d term=none cterm=none ctermfg=2
hi Question gui=bold guifg=#a8a030 cterm=bold ctermfg=11
hi MoreMsg guifg=#96a42d ctermfg=10
hi ModeMsg gui=bold guifg=#8c9e3d cterm=bold ctermfg=2

hi VimOption guifg=#b58d88 ctermfg=5
hi VimGroup guifg=#b58d88 ctermfg=5

hi Underlined gui=underline,bold guifg=#aeadaf cterm=underline,bold ctermfg=15
hi Ignore guibg=#312e30 guifg=#949d9f ctermbg=8 ctermfg=7
hi Conceal guibg=#949d9f guifg=#312e30 ctermbg=7 ctermfg=8

hi SpellBad guibg=#d2813d guifg=#232323 ctermbg=1 ctermfg=0
hi SpellCap guibg=#b1942b guifg=#232323 ctermbg=3 ctermfg=0
hi SpellRare guibg=#d58888 guifg=#232323 ctermbg=13 ctermfg=0
hi SpellLocal guibg=#7aa880 guifg=#232323 ctermbg=14 ctermfg=0

hi Pmenu gui=italic guibg=#312e30 guifg=#aeadaf cterm=italic ctermbg=8 ctermfg=15
hi PmenuSel gui=none,bold guibg=#949d9f guifg=#232323 cterm=none,bold ctermbg=7 ctermfg=0
hi PmenuSbar guibg=#312e30 ctermbg=8
hi PmenuThumb guibg=#949d9f ctermbg=7

" Diffs
" -----------------
hi DiffAdd gui=bold guibg=#8c9e3d guifg=#232323 cterm=bold ctermbg=10 ctermfg=0
hi DiffDelete gui=none guibg=#d2813d guifg=#232323 cterm=none ctermbg=9 ctermfg=0
hi DiffChange gui=bold guibg=#312e30 guifg=#949d9f cterm=bold ctermbg=8 ctermfg=7
hi DiffText gui=bold guibg=#312e30 guifg=#d0913d cterm=bold ctermbg=8 ctermfg=1

hi diffAdded guifg=#8c9e3d ctermfg=2
hi diffRemoved guifg=#d2813d ctermfg=1
hi diffNewFile gui=none guifg=#6e9cb0 ctermfg=4
hi diffFile gui=none guifg=#b1942b cterm=none ctermfg=3
