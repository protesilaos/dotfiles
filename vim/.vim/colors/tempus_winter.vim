" Name: Tempus Winter
" Author: Protesilaos Stavrou (https://protesilaos.com)
" Description: Dark theme with a palette inspired by winter nights at the city (WCAG AA compliant)

set background=dark
hi clear
if exists("syntax_on")
  syntax reset
endif
let g:colors_name = "tempus_winter"

" General
" -----------------

" NOTE the ctermbg=none is for terminals with transparency
hi Normal guibg=#202427 guifg=#8da3b8 ctermbg=none ctermfg=15
hi Visual guibg=#8da3b8 guifg=#202427 ctermbg=15 ctermfg=0
hi Search gui=underline,bold,italic guibg=#30a480 guifg=#202427 cterm=underline,bold,italic ctermbg=2 ctermfg=0
hi IncSearch gui=underline,bold,italic guibg=#909294 guifg=#202427 term=none cterm=underline,bold,italic ctermbg=7 ctermfg=0

hi StatusLine gui=none,bold guibg=#8da3b8 guifg=#202427 cterm=none,bold ctermbg=15 ctermfg=0
hi StatusLineNC gui=none guibg=#292b35 guifg=#909294 cterm=none ctermbg=8 ctermfg=7
hi StatusLineTerm gui=none,bold guibg=#30a480 guifg=#202427 cterm=none,bold ctermbg=2 ctermfg=0
hi StatusLineTermNC gui=none guibg=#292b35 guifg=#30a480 cterm=none ctermbg=8 ctermfg=2

hi VertSplit gui=none cterm=none
hi TabLine gui=none guibg=#292b35 guifg=#909294 cterm=none ctermbg=8 ctermfg=7
hi TabLineSel gui=none guibg=#8da3b8 guifg=#202427 cterm=none ctermbg=15 ctermfg=0
hi TabLineFill gui=none cterm=none

hi Comment gui=italic guifg=#909294 cterm=italic ctermfg=7
hi Todo gui=bold guibg=#292b35 guifg=#ad8e4b cterm=bold ctermbg=8 ctermfg=11

hi Warning gui=none guibg=#959721 guifg=#202427 cterm=none ctermbg=3 ctermfg=0
hi WarningMsg gui=none guibg=#959721 guifg=#202427 cterm=none ctermbg=3 ctermfg=0
hi Error gui=none guibg=#ed6a42 guifg=#202427 cterm=none ctermbg=1 ctermfg=0
hi ErrorMsg gui=none guibg=#ed6a42 guifg=#202427 cterm=none ctermbg=1 ctermfg=0

hi MatchParen gui=underline,bold guibg=#c874c2 guifg=#202427 cterm=underline,bold ctermbg=13 ctermfg=0

hi ToolbarLine guibg=#909294 guifg=#202427 term=none ctermbg=7 ctermfg=0
hi ToolbarButton gui=bold guibg=#909294 guifg=#202427 term=none cterm=bold ctermbg=7 ctermfg=0

hi WildMenu guibg=#202427 guifg=#8da3b8 term=standout ctermbg=0 ctermfg=15

hi Terminal guibg=#202427 guifg=#8da3b8 term=none ctermbg=0 ctermfg=15

" Constructs
" -----------------
hi Constant guifg=#2b9dc4 ctermfg=4
hi Number guifg=#2b9dc4 ctermfg=4
hi Float guifg=#2b9dc4 ctermfg=4
hi String guifg=#798fd7 ctermfg=12

hi Function guifg=#bd7bbe ctermfg=5
hi Identifier guifg=#c874c2 term=none ctermfg=13
hi Label guifg=#bd7bbe ctermfg=5
hi Tag guifg=#bd7bbe ctermfg=5
hi Keyword gui=bold guifg=#c874c2 gui=bold ctermfg=13

hi Character gui=bold guifg=#0fa674 cterm=bold ctermfg=14

hi Type gui=none,bold guifg=#2fa1a0 term=none cterm=none,bold ctermfg=6
hi Boolean guifg=#2fa1a0 ctermfg=6
hi StorageClass guifg=#2fa1a0 ctermfg=6
hi Structure guifg=#2fa1a0 ctermfg=6
hi Typedef gui=bold guifg=#0fa674 cterm=bold ctermfg=14

hi Conditional gui=bold guifg=#30a480 cterm=bold ctermfg=2
hi Statement gui=none guifg=#43a550 cterm=none ctermfg=10
hi Repeat gui=bold guifg=#43a550 cterm=bold ctermfg=10
hi Operator gui=bold guifg=#8da3b8 cterm=bold ctermfg=15
hi Exception gui=bold guifg=#ed6a42 cterm=bold ctermfg=1

hi Preproc gui=none guifg=#db7824 term=none cterm=none ctermfg=9
hi PreCondit gui=bold guifg=#db7824 cterm=bold ctermfg=9
hi Macro gui=bold guifg=#db7824 cterm=bold ctermfg=9
hi Include guifg=#db7824 ctermfg=9
hi Define guifg=#db7824 ctermfg=9

hi Title gui=bold guibg=#202427 guifg=#2fa1a0 cterm=bold ctermbg=0 ctermfg=6

hi Special gui=bold guifg=#ad8e4b term=none cterm=bold ctermfg=11
hi SpecialKey guifg=#ad8e4b ctermfg=11
hi SpecialChar gui=bold guifg=#ad8e4b cterm=bold ctermfg=11

hi Delimeter gui=bold guifg=#bd7bbe cterm=bold ctermfg=5
hi Delimiter gui=bold guifg=#bd7bbe cterm=bold ctermfg=5
hi SpecialComment gui=bold guifg=#bd7bbe cterm=bold ctermfg=5

hi Debug guifg=#c874c2 ctermfg=13

" Other
" -----------------
hi LineNr guibg=#292b35 guifg=#909294 term=none ctermbg=8 ctermfg=7
hi Cursor guibg=#8da3b8 guifg=#202427 ctermbg=15 ctermfg=0
hi CursorLine gui=none guibg=NONE term=none cterm=none ctermbg=none
hi CursorColumn gui=none guibg=#292b35 term=none cterm=none ctermbg=8
hi CursorLineNr gui=bold guibg=#909294 guifg=#202427 cterm=bold ctermbg=7 ctermfg=0
hi ColorColumn guibg=#292b35 guifg=#8da3b8 term=none ctermbg=8 ctermfg=15

hi Folded guibg=#292b35 guifg=#909294 ctermbg=8 ctermfg=7
hi FoldColumn guibg=#292b35 guifg=#909294 ctermbg=8 ctermfg=7

hi NonText gui=bold guibg=NONE guifg=#909294 cterm=bold ctermbg=none ctermfg=7

hi Directory gui=none guifg=#30a480 term=none cterm=none ctermfg=2
hi Question gui=bold guifg=#ad8e4b cterm=bold ctermfg=11
hi MoreMsg guifg=#43a550 ctermfg=10
hi ModeMsg gui=bold guifg=#30a480 cterm=bold ctermfg=2

hi VimOption guifg=#bd7bbe ctermfg=5
hi VimGroup guifg=#bd7bbe ctermfg=5

hi Underlined gui=underline,bold guifg=#8da3b8 cterm=underline,bold ctermfg=15
hi Ignore guibg=#292b35 guifg=#909294 ctermbg=8 ctermfg=7
hi Conceal guibg=#909294 guifg=#292b35 ctermbg=7 ctermfg=8

hi SpellBad guibg=#ed6a42 guifg=#202427 ctermbg=1 ctermfg=0
hi SpellCap guibg=#959721 guifg=#202427 ctermbg=3 ctermfg=0
hi SpellRare guibg=#c874c2 guifg=#202427 ctermbg=13 ctermfg=0
hi SpellLocal guibg=#0fa674 guifg=#202427 ctermbg=14 ctermfg=0

hi Pmenu gui=italic guibg=#292b35 guifg=#8da3b8 cterm=italic ctermbg=8 ctermfg=15
hi PmenuSel gui=none,bold guibg=#909294 guifg=#202427 cterm=none,bold ctermbg=7 ctermfg=0
hi PmenuSbar guibg=#292b35 ctermbg=8
hi PmenuThumb guibg=#909294 ctermbg=7

" Diffs
" -----------------
hi DiffAdd gui=bold guibg=#30a480 guifg=#202427 cterm=bold ctermbg=10 ctermfg=0
hi DiffDelete gui=none guibg=#ed6a42 guifg=#202427 cterm=none ctermbg=9 ctermfg=0
hi DiffChange gui=bold guibg=#292b35 guifg=#909294 cterm=bold ctermbg=8 ctermfg=7
hi DiffText gui=bold guibg=#292b35 guifg=#db7824 cterm=bold ctermbg=8 ctermfg=1

hi diffAdded guifg=#30a480 ctermfg=2
hi diffRemoved guifg=#ed6a42 ctermfg=1
hi diffNewFile gui=none guifg=#2b9dc4 ctermfg=4
hi diffFile gui=none guifg=#959721 cterm=none ctermfg=3
