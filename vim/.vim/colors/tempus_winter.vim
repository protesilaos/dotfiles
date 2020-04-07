" Name: Tempus Winter
" Description: Dark theme with a palette inspired by winter nights at the city (WCAG AA compliant)
" Author: Protesilaos Stavrou (https://protesilaos.com)
" Meta: Created with the Tempus Themes Generator
" URL: https://gitlab.com/protesilaos/tempus-themes-generator

set background=dark
hi clear
if exists("syntax_on")
	syntax reset
endif
let g:colors_name = "tempus_winter"

" General
" -------
" NOTE the ctermbg=none is for terminals with transparency
hi Normal guibg=#202427 guifg=#8da3b8 ctermbg=none ctermfg=15
hi Visual guibg=#8da3b8 guifg=#202427 ctermbg=15 ctermfg=0
hi Search gui=underline,bold guibg=#292b35 guifg=#8da3b8 cterm=underline,bold ctermbg=8 ctermfg=3
hi IncSearch gui=underline,bold guibg=#909294 guifg=#202427 term=none cterm=underline,bold ctermbg=7 ctermfg=0

hi StatusLine gui=none guibg=#8da3b8 guifg=#202427 cterm=none ctermbg=15 ctermfg=0
hi StatusLineNC gui=none guibg=#292b35 guifg=#909294 cterm=none ctermbg=8 ctermfg=7
hi StatusLineTerm gui=none guibg=#49a61d guifg=#202427 cterm=none ctermbg=2 ctermfg=0
hi StatusLineTermNC gui=none guibg=#292b35 guifg=#49a61d cterm=none ctermbg=8 ctermfg=2

hi VertSplit gui=none cterm=none
hi TabLine gui=none guibg=#292b35 guifg=#909294 cterm=none ctermbg=8 ctermfg=7
hi TabLineSel gui=none guibg=#4fa090 guifg=#202427 cterm=none ctermbg=6 ctermfg=0
hi TabLineFill gui=none cterm=none

hi Comment gui=italic guifg=#909294 cterm=none ctermfg=7
hi Todo gui=bold guibg=#292b35 guifg=#ad8e4b cterm=bold ctermbg=8 ctermfg=11

hi Warning gui=none guibg=#959721 guifg=#202427 cterm=none ctermbg=3 ctermfg=0
hi WarningMsg gui=none guibg=#959721 guifg=#202427 cterm=none ctermbg=3 ctermfg=0
hi Error gui=none guibg=#eb6a58 guifg=#202427 cterm=none ctermbg=1 ctermfg=0
hi ErrorMsg gui=none guibg=#eb6a58 guifg=#202427 cterm=none ctermbg=1 ctermfg=0

hi MatchParen gui=underline,bold guibg=#292b35 guifg=#909294 cterm=underline,bold ctermbg=8 ctermfg=7

hi ToolbarLine guibg=#909294 guifg=#202427 term=none ctermbg=7 ctermfg=0
hi ToolbarButton gui=bold guibg=#909294 guifg=#202427 term=none cterm=bold ctermbg=7 ctermfg=0

hi WildMenu guibg=#202427 guifg=#8da3b8 term=standout ctermbg=0 ctermfg=15

hi Terminal guibg=#202427 guifg=#8da3b8 term=none ctermbg=0 ctermfg=15

" Constructs
" ----------
hi Constant guifg=#798fd7 ctermfg=4
hi Number guifg=#798fd7 ctermfg=4
hi Float guifg=#798fd7 ctermfg=4
hi String guifg=#309dc1 ctermfg=12

hi Function guifg=#cd7b7e ctermfg=5
hi Identifier guifg=#c874c2 term=none ctermfg=13
hi Label guifg=#cd7b7e ctermfg=5
hi Tag guifg=#cd7b7e ctermfg=5
hi Keyword gui=bold guifg=#c874c2 gui=bold ctermfg=13

hi Character gui=bold guifg=#1ba2a0 cterm=bold ctermfg=14

hi Type gui=none,bold guifg=#4fa090 term=none cterm=none,bold ctermfg=6
hi Boolean guifg=#4fa090 ctermfg=6
hi StorageClass guifg=#4fa090 ctermfg=6
hi Structure guifg=#4fa090 ctermfg=6
hi Typedef gui=bold guifg=#1ba2a0 cterm=bold ctermfg=14

hi Conditional gui=bold guifg=#49a61d cterm=bold ctermfg=2
hi Statement gui=none guifg=#00a854 cterm=none ctermfg=10
hi Repeat gui=bold guifg=#00a854 cterm=bold ctermfg=10
hi Operator gui=bold guifg=#8da3b8 cterm=bold ctermfg=15
hi Exception gui=bold guifg=#eb6a58 cterm=bold ctermfg=1

hi Preproc gui=none guifg=#db7824 term=none cterm=none ctermfg=9
hi PreCondit gui=bold guifg=#db7824 cterm=bold ctermfg=9
hi Macro gui=bold guifg=#db7824 cterm=bold ctermfg=9
hi Include guifg=#db7824 ctermfg=9
hi Define guifg=#db7824 ctermfg=9

hi Title gui=bold guibg=#202427 guifg=#4fa090 cterm=bold ctermbg=0 ctermfg=6

hi Delimeter gui=bold guifg=#cd7b7e cterm=bold ctermfg=5
hi Delimiter gui=bold guifg=#cd7b7e cterm=bold ctermfg=5
hi SpecialComment gui=bold guifg=#cd7b7e cterm=bold ctermfg=5

hi Debug guifg=#c874c2 ctermfg=13

" Other
" -----
hi LineNr guibg=#292b35 guifg=#909294 term=none ctermbg=8 ctermfg=7
hi Cursor guibg=#8da3b8 guifg=#202427 ctermbg=15 ctermfg=0
hi CursorLine gui=none guibg=#292b35 term=none cterm=none ctermbg=8
hi CursorColumn gui=none guibg=#292b35 term=none cterm=none ctermbg=8
hi CursorLineNr gui=bold guibg=#909294 guifg=#202427 cterm=bold ctermbg=7 ctermfg=0
hi ColorColumn guibg=#292b35 guifg=#8da3b8 term=none ctermbg=8 ctermfg=15
hi SignColumn guibg=#292b35 guifg=#909294 term=none ctermbg=8 ctermfg=7

hi Folded guibg=#292b35 guifg=#909294 ctermbg=8 ctermfg=7
hi FoldColumn guibg=#292b35 guifg=#909294 ctermbg=8 ctermfg=7

hi Special gui=bold guifg=#ad8e4b term=none cterm=bold ctermfg=11
hi SpecialKey gui=none guibg=#292b35 guifg=#909294 cterm=none ctermbg=8 ctermfg=7
hi SpecialChar gui=bold guifg=#ad8e4b cterm=bold ctermfg=11
hi NonText gui=none guibg=#292b35 guifg=#909294 cterm=none ctermbg=8 ctermfg=7
hi EndOfBuffer gui=bold guifg=#909294 cterm=bold ctermfg=7

hi Directory gui=none guifg=#49a61d term=none cterm=none ctermfg=2
hi Question gui=bold guifg=#ad8e4b cterm=bold ctermfg=11
hi MoreMsg guifg=#00a854 ctermfg=10
hi ModeMsg gui=bold guifg=#49a61d cterm=bold ctermfg=2

hi VimOption guifg=#cd7b7e ctermfg=5
hi VimGroup guifg=#cd7b7e ctermfg=5

hi Underlined gui=underline guifg=#8da3b8 cterm=underline ctermfg=15
hi Ignore guibg=#292b35 guifg=#909294 ctermbg=8 ctermfg=7
hi Conceal guibg=#909294 guifg=#292b35 ctermbg=7 ctermfg=8

hi SpellBad guibg=#eb6a58 guifg=#202427 ctermbg=1 ctermfg=0
hi SpellCap guibg=#959721 guifg=#202427 ctermbg=3 ctermfg=0
hi SpellRare guibg=#c874c2 guifg=#202427 ctermbg=13 ctermfg=0
hi SpellLocal guibg=#1ba2a0 guifg=#202427 ctermbg=14 ctermfg=0

hi Pmenu gui=italic guibg=#292b35 guifg=#8da3b8 cterm=none ctermbg=8 ctermfg=15
hi PmenuSel gui=none,bold guibg=#909294 guifg=#202427 cterm=none,bold ctermbg=7 ctermfg=0
hi PmenuSbar guibg=#292b35 ctermbg=8
hi PmenuThumb guibg=#909294 ctermbg=7

" Diffs
" -----
hi DiffAdd gui=bold guibg=#49a61d guifg=#202427 cterm=bold ctermbg=10 ctermfg=0
hi DiffDelete gui=none guibg=#eb6a58 guifg=#202427 cterm=none ctermbg=9 ctermfg=0
hi DiffChange gui=bold guibg=#292b35 guifg=#909294 cterm=bold ctermbg=8 ctermfg=7
hi DiffText gui=bold guibg=#292b35 guifg=#db7824 cterm=bold ctermbg=8 ctermfg=1

hi diffAdded guifg=#49a61d ctermfg=2
hi diffRemoved guifg=#eb6a58 ctermfg=1
hi diffNewFile gui=none guifg=#798fd7 ctermfg=4
hi diffFile gui=none guifg=#959721 cterm=none ctermfg=3

hi GitGutterAdd guibg=#292b35 guifg=#49a61d ctermbg=8 ctermfg=2
hi GitGutterChange gui=bold guibg=#292b35 guifg=#909294 cterm=bold ctermbg=8 ctermfg=7
hi GitGutterDelete guibg=#292b35 guifg=#eb6a58 ctermbg=8 ctermfg=1
hi GitGutterChangeDelete gui=bold guibg=#292b35 guifg=#eb6a58 cterm=bold ctermbg=8 ctermfg=1

" Neomake
" -------
hi NeomakeError gui=none guibg=#eb6a58 guifg=#202427 cterm=none ctermbg=1 ctermfg=0
hi NeomakeInfo gui=none guibg=#4fa090 guifg=#202427 cterm=none ctermbg=6 ctermfg=0
hi NeomakeWarning gui=none guibg=#959721 guifg=#202427 cterm=none ctermbg=3 ctermfg=0
hi NeomakeMessage gui=none guibg=#ad8e4b guifg=#202427 cterm=none ctermbg=11 ctermfg=0

hi NeomakeVirtualtextInfoDefault guifg=#1ba2a0 ctermfg=14
hi NeomakeVirtualtextMessageDefault guifg=#ad8e4b ctermfg=11
hi NeomakeVirtualtextWarningDefault guifg=#959721 ctermfg=3
hi NeomakeVirtualtextErrorDefault guifg=#eb6a58 ctermfg=1

hi NeomakeStatusGood gui=none guibg=#49a61d guifg=#202427 cterm=none ctermbg=2 ctermfg=0
hi NeomakeStatusGoodNC gui=none guibg=#292b35 guifg=#49a61d cterm=none ctermbg=8 ctermfg=2

hi NeomakeStatColorDefault gui=none guibg=#798fd7 guifg=#202427 cterm=none ctermbg=4 ctermfg=0
hi NeomakeStatColorTypeE gui=none guibg=#eb6a58 guifg=#202427 cterm=none ctermbg=1 ctermfg=0
hi NeomakeStatColorTypeW gui=none guibg=#959721 guifg=#202427 cterm=none ctermbg=3 ctermfg=0

" Markdown
" --------
hi MarkdownRule gui=bold guibg=#292b35 guifg=#00a854 cterm=bold ctermbg=8 ctermfg=10

hi MarkdownHeading gui=bold guifg=#8da3b8 cterm=bold ctermfg=15
hi default link MarkdownH1 MarkdownHeading
hi default link MarkdownH2 MarkdownHeading
hi default link MarkdownH3 MarkdownHeading
hi default link MarkdownH4 MarkdownHeading
hi default link MarkdownH5 MarkdownHeading
hi default link MarkdownH6 MarkdownHeading
hi default link MarkdownHeadingDelimiter MarkdownHeading
hi default link MarkdownHeadingRule MarkdownHeading

hi MarkdownBold gui=bold guifg=#db7824 cterm=bold ctermfg=9
hi default link MarkdownBoldDelimiter MarkdownBold

hi MarkdownItalic gui=italic guifg=#959721 cterm=none ctermfg=3
hi default link MarkdownItalicDelimiter MarkdownItalic

hi MarkdownUrl gui=underline guifg=#798fd7 cterm=underline ctermfg=4
hi MarkdownLinkText gui=none guifg=#309dc1 cterm=none ctermfg=12
hi MarkdownLinkDelimiter gui=bold guifg=#8da3b8 cterm=bold ctermfg=15
hi default link MarkdownLinkTextDelimiter MarkdownLinkDelimiter

hi MarkdownCode guifg=#cd7b7e ctermfg=5
hi default link MarkdownCodeDelimiter MarkdownCode

hi MarkdownCodeBlock guifg=#8da3b8 ctermfg=15

hi MarkdownListMarker gui=none guifg=#49a61d cterm=none ctermfg=2
hi default link MarkdownOrderedListMarker MarkdownListMarker

" Linting
" -------
hi YcmErrorSection gui=undercurl guisp=#eb6a58 cterm=underline
hi YcmWarningSection gui=undercurl guisp=#959721 cterm=underline
hi SyntasticError gui=undercurl guisp=#eb6a58 cterm=underline
hi SyntasticWarning gui=undercurl guisp=#959721 cterm=underline
hi SyntasticErrorSing guifg=#202427 guibg=#eb6a58 ctermfg=0 ctermbg=1
hi SyntasticWarningSign guifg=#202427 guibg=#959721 ctermfg=0 ctermbg=3
