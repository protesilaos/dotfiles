" Name: Tempus Day
" Author: Protesilaos Stavrou (https://protesilaos.com)
" Description: Light theme with warm colours (WCAG AA compliant)

set background=light
hi clear
if exists("syntax_on")
  syntax reset
endif
let g:colors_name = "tempus_day"

" General
" -----------------

" NOTE the ctermbg=none is for terminals with transparency
hi Normal guibg=#f8f1e0 guifg=#3b2820 ctermbg=none ctermfg=0
hi Visual guibg=#3b2820 guifg=#f8f1e0 ctermbg=0 ctermfg=15
hi Search gui=underline,bold,italic guibg=#207f20 guifg=#f8f1e0 cterm=underline,bold,italic ctermbg=2 ctermfg=15
hi IncSearch gui=underline,bold,italic guibg=#5b4440 guifg=#f8f1e0 term=none cterm=underline,bold,italic ctermbg=8 ctermfg=15

hi StatusLine gui=none,bold guibg=#3b2820 guifg=#f8f1e0 cterm=none,bold ctermbg=0 ctermfg=15
hi StatusLineNC gui=none guibg=#f5f0f5 guifg=#5b4440 cterm=none ctermbg=7 ctermfg=8
hi StatusLineTerm gui=none,bold guibg=#207f20 guifg=#f8f1e0 cterm=none,bold ctermbg=2 ctermfg=15
hi StatusLineTermNC gui=none guibg=#f5f0f5 guifg=#207f20 cterm=none ctermbg=7 ctermfg=2

hi VertSplit gui=none cterm=none
hi TabLine gui=none guibg=#f5f0f5 guifg=#5b4440 cterm=none ctermbg=7 ctermfg=8
hi TabLineSel gui=none guibg=#3b2820 guifg=#f8f1e0 cterm=none ctermbg=0 ctermfg=15
hi TabLineFill gui=none cterm=none

hi Comment gui=italic guifg=#5b4440 cterm=italic ctermfg=8
hi Todo gui=bold guibg=#f5f0f5 guifg=#9a6033 cterm=bold ctermbg=7 ctermfg=11

hi Warning gui=none guibg=#8a6900 guifg=#f8f1e0 cterm=none ctermbg=3 ctermfg=15
hi WarningMsg gui=none guibg=#8a6900 guifg=#f8f1e0 cterm=none ctermbg=3 ctermfg=15
hi Error gui=none guibg=#d62500 guifg=#f8f1e0 cterm=none ctermbg=1 ctermfg=15
hi ErrorMsg gui=none guibg=#d62500 guifg=#f8f1e0 cterm=none ctermbg=1 ctermfg=15

hi MatchParen gui=underline,bold guibg=#8850d0 guifg=#f8f1e0 cterm=underline,bold ctermbg=13 ctermfg=15

hi ToolbarLine guibg=#5b4440 guifg=#f8f1e0 term=none ctermbg=8 ctermfg=15
hi ToolbarButton gui=bold guibg=#5b4440 guifg=#f8f1e0 term=none cterm=bold ctermbg=8 ctermfg=15

hi WildMenu guibg=#f8f1e0 guifg=#3b2820 term=standout ctermbg=15 ctermfg=0

hi Terminal guibg=#f8f1e0 guifg=#3b2820 term=none ctermbg=15 ctermfg=0

" Constructs
" -----------------
hi Constant guifg=#306ad0 ctermfg=4
hi Number guifg=#306ad0 ctermfg=4
hi Float guifg=#306ad0 ctermfg=4
hi String guifg=#6061d0 ctermfg=12

hi Function guifg=#c6365d ctermfg=5
hi Identifier guifg=#8850d0 term=none ctermfg=13
hi Label guifg=#c6365d ctermfg=5
hi Tag guifg=#c6365d ctermfg=5
hi Keyword gui=bold guifg=#8850d0 gui=bold ctermfg=13

hi Character gui=bold guifg=#007c6d cterm=bold ctermfg=14

hi Type gui=none,bold guifg=#00798d term=none cterm=none,bold ctermfg=6
hi Boolean guifg=#00798d ctermfg=6
hi StorageClass guifg=#00798d ctermfg=6
hi Structure guifg=#00798d ctermfg=6
hi Typedef gui=bold guifg=#007c6d cterm=bold ctermfg=14

hi Conditional gui=bold guifg=#207f20 cterm=bold ctermfg=2
hi Statement gui=none guifg=#5a7800 cterm=none ctermfg=10
hi Repeat gui=bold guifg=#5a7800 cterm=bold ctermfg=10
hi Operator gui=bold guifg=#3b2820 cterm=bold ctermfg=0
hi Exception gui=bold guifg=#d62500 cterm=bold ctermfg=1

hi Preproc gui=none guifg=#bb4b00 term=none cterm=none ctermfg=9
hi PreCondit gui=bold guifg=#bb4b00 cterm=bold ctermfg=9
hi Macro gui=bold guifg=#bb4b00 cterm=bold ctermfg=9
hi Include guifg=#bb4b00 ctermfg=9
hi Define guifg=#bb4b00 ctermfg=9

hi Title gui=bold guibg=#f8f1e0 guifg=#00798d cterm=bold ctermbg=15 ctermfg=6

hi Special gui=bold guifg=#9a6033 term=none cterm=bold ctermfg=11
hi SpecialKey guifg=#9a6033 ctermfg=11
hi SpecialChar gui=bold guifg=#9a6033 cterm=bold ctermfg=11

hi Delimeter gui=bold guifg=#c6365d cterm=bold ctermfg=5
hi Delimiter gui=bold guifg=#c6365d cterm=bold ctermfg=5
hi SpecialComment gui=bold guifg=#c6365d cterm=bold ctermfg=5

hi Debug guifg=#8850d0 ctermfg=13

" Other
" -----------------
hi LineNr guibg=#f5f0f5 guifg=#5b4440 term=none ctermbg=7 ctermfg=8
hi Cursor guibg=#3b2820 guifg=#f8f1e0 ctermbg=0 ctermfg=15
hi CursorLine gui=none guibg=NONE term=none cterm=none ctermbg=none
hi CursorColumn gui=none guibg=#f5f0f5 term=none cterm=none ctermbg=7
hi CursorLineNr gui=bold guibg=#5b4440 guifg=#f8f1e0 cterm=bold ctermbg=8 ctermfg=15
hi ColorColumn guibg=#f5f0f5 guifg=#3b2820 term=none ctermbg=7 ctermfg=0

hi Folded guibg=#f5f0f5 guifg=#5b4440 ctermbg=7 ctermfg=8
hi FoldColumn guibg=#f5f0f5 guifg=#5b4440 ctermbg=7 ctermfg=8

hi NonText gui=bold guibg=NONE guifg=#5b4440 cterm=bold ctermbg=none ctermfg=8

hi Directory gui=none guifg=#207f20 term=none cterm=none ctermfg=2
hi Question gui=bold guifg=#9a6033 cterm=bold ctermfg=11
hi MoreMsg guifg=#5a7800 ctermfg=10
hi ModeMsg gui=bold guifg=#207f20 cterm=bold ctermfg=2

hi VimOption guifg=#c6365d ctermfg=5
hi VimGroup guifg=#c6365d ctermfg=5

hi Underlined gui=underline,bold guifg=#3b2820 cterm=underline,bold ctermfg=0
hi Ignore guibg=#f5f0f5 guifg=#5b4440 ctermbg=7 ctermfg=8
hi Conceal guibg=#5b4440 guifg=#f5f0f5 ctermbg=8 ctermfg=7

hi SpellBad guibg=#d62500 guifg=#f8f1e0 ctermbg=1 ctermfg=15
hi SpellCap guibg=#8a6900 guifg=#f8f1e0 ctermbg=3 ctermfg=15
hi SpellRare guibg=#8850d0 guifg=#f8f1e0 ctermbg=13 ctermfg=15
hi SpellLocal guibg=#007c6d guifg=#f8f1e0 ctermbg=14 ctermfg=15

hi Pmenu gui=italic guibg=#f5f0f5 guifg=#3b2820 cterm=italic ctermbg=7 ctermfg=0
hi PmenuSel gui=none,bold guibg=#5b4440 guifg=#f8f1e0 cterm=none,bold ctermbg=8 ctermfg=15
hi PmenuSbar guibg=#f5f0f5 ctermbg=7
hi PmenuThumb guibg=#5b4440 ctermbg=8

" Diffs
" -----------------
hi DiffAdd gui=bold guibg=#207f20 guifg=#f8f1e0 cterm=bold ctermbg=10 ctermfg=15
hi DiffDelete gui=none guibg=#d62500 guifg=#f8f1e0 cterm=none ctermbg=9 ctermfg=15
hi DiffChange gui=bold guibg=#f5f0f5 guifg=#5b4440 cterm=bold ctermbg=7 ctermfg=8
hi DiffText gui=bold guibg=#f5f0f5 guifg=#bb4b00 cterm=bold ctermbg=7 ctermfg=1

hi diffAdded guifg=#207f20 ctermfg=2
hi diffRemoved guifg=#d62500 ctermfg=1
hi diffNewFile gui=none guifg=#306ad0 ctermfg=4
hi diffFile gui=none guifg=#8a6900 cterm=none ctermfg=3
