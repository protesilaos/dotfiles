" Name: Tempus Day
" Description: Light theme with warm colours (WCAG AA compliant)
" Author: Protesilaos Stavrou (https://protesilaos.com)
" Meta: Created with the Tempus Themes Generator
" URL: https://gitlab.com/protesilaos/tempus-themes-generator

set background=light
hi clear
if exists("syntax_on")
	syntax reset
endif
let g:colors_name = "tempus_day"

" General
" -------
" NOTE the ctermbg=none is for terminals with transparency
hi Normal guibg=#f8f2e5 guifg=#464340 ctermbg=none ctermfg=0
hi Visual guibg=#464340 guifg=#f8f2e5 ctermbg=0 ctermfg=15
hi Search gui=underline,bold guibg=#eae9dd guifg=#464340 cterm=underline,bold ctermbg=7 ctermfg=3
hi IncSearch gui=underline,bold guibg=#68607d guifg=#f8f2e5 term=none cterm=underline,bold ctermbg=8 ctermfg=15

hi StatusLine gui=none guibg=#464340 guifg=#f8f2e5 cterm=none ctermbg=0 ctermfg=15
hi StatusLineNC gui=none guibg=#eae9dd guifg=#68607d cterm=none ctermbg=7 ctermfg=8
hi StatusLineTerm gui=none guibg=#107410 guifg=#f8f2e5 cterm=none ctermbg=2 ctermfg=15
hi StatusLineTermNC gui=none guibg=#eae9dd guifg=#107410 cterm=none ctermbg=7 ctermfg=2

hi VertSplit gui=none cterm=none
hi TabLine gui=none guibg=#eae9dd guifg=#68607d cterm=none ctermbg=7 ctermfg=8
hi TabLineSel gui=none guibg=#007070 guifg=#f8f2e5 cterm=none ctermbg=6 ctermfg=15
hi TabLineFill gui=none cterm=none

hi Comment gui=italic guifg=#68607d cterm=none ctermfg=8
hi Todo gui=bold guibg=#eae9dd guifg=#706a00 cterm=bold ctermbg=7 ctermfg=11

hi Warning gui=none guibg=#806000 guifg=#f8f2e5 cterm=none ctermbg=3 ctermfg=15
hi WarningMsg gui=none guibg=#806000 guifg=#f8f2e5 cterm=none ctermbg=3 ctermfg=15
hi Error gui=none guibg=#c81000 guifg=#f8f2e5 cterm=none ctermbg=1 ctermfg=15
hi ErrorMsg gui=none guibg=#c81000 guifg=#f8f2e5 cterm=none ctermbg=1 ctermfg=15

hi MatchParen gui=underline,bold guibg=#eae9dd guifg=#68607d cterm=underline,bold ctermbg=7 ctermfg=8

hi ToolbarLine guibg=#68607d guifg=#f8f2e5 term=none ctermbg=8 ctermfg=15
hi ToolbarButton gui=bold guibg=#68607d guifg=#f8f2e5 term=none cterm=bold ctermbg=8 ctermfg=15

hi WildMenu guibg=#f8f2e5 guifg=#464340 term=standout ctermbg=15 ctermfg=0

hi Terminal guibg=#f8f2e5 guifg=#464340 term=none ctermbg=15 ctermfg=0

" Constructs
" ----------
hi Constant guifg=#385dc4 ctermfg=4
hi Number guifg=#385dc4 ctermfg=4
hi Float guifg=#385dc4 ctermfg=4
hi String guifg=#0d66c9 ctermfg=12

hi Function guifg=#b63052 ctermfg=5
hi Identifier guifg=#8055aa term=none ctermfg=13
hi Label guifg=#b63052 ctermfg=5
hi Tag guifg=#b63052 ctermfg=5
hi Keyword gui=bold guifg=#8055aa gui=bold ctermfg=13

hi Character gui=bold guifg=#337087 cterm=bold ctermfg=14

hi Type gui=none,bold guifg=#007070 term=none cterm=none,bold ctermfg=6
hi Boolean guifg=#007070 ctermfg=6
hi StorageClass guifg=#007070 ctermfg=6
hi Structure guifg=#007070 ctermfg=6
hi Typedef gui=bold guifg=#337087 cterm=bold ctermfg=14

hi Conditional gui=bold guifg=#107410 cterm=bold ctermfg=2
hi Statement gui=none guifg=#4a7240 cterm=none ctermfg=10
hi Repeat gui=bold guifg=#4a7240 cterm=bold ctermfg=10
hi Operator gui=bold guifg=#464340 cterm=bold ctermfg=0
hi Exception gui=bold guifg=#c81000 cterm=bold ctermfg=1

hi Preproc gui=none guifg=#b94000 term=none cterm=none ctermfg=9
hi PreCondit gui=bold guifg=#b94000 cterm=bold ctermfg=9
hi Macro gui=bold guifg=#b94000 cterm=bold ctermfg=9
hi Include guifg=#b94000 ctermfg=9
hi Define guifg=#b94000 ctermfg=9

hi Title gui=bold guibg=#f8f2e5 guifg=#007070 cterm=bold ctermbg=15 ctermfg=6

hi Delimeter gui=bold guifg=#b63052 cterm=bold ctermfg=5
hi Delimiter gui=bold guifg=#b63052 cterm=bold ctermfg=5
hi SpecialComment gui=bold guifg=#b63052 cterm=bold ctermfg=5

hi Debug guifg=#8055aa ctermfg=13

" Other
" -----
hi LineNr guibg=#eae9dd guifg=#68607d term=none ctermbg=7 ctermfg=8
hi Cursor guibg=#464340 guifg=#f8f2e5 ctermbg=0 ctermfg=15
hi CursorLine gui=none guibg=#eae9dd term=none cterm=none ctermbg=7
hi CursorColumn gui=none guibg=#eae9dd term=none cterm=none ctermbg=7
hi CursorLineNr gui=bold guibg=#68607d guifg=#f8f2e5 cterm=bold ctermbg=8 ctermfg=15
hi ColorColumn guibg=#eae9dd guifg=#464340 term=none ctermbg=7 ctermfg=0
hi SignColumn guibg=#eae9dd guifg=#68607d term=none ctermbg=7 ctermfg=8

hi Folded guibg=#eae9dd guifg=#68607d ctermbg=7 ctermfg=8
hi FoldColumn guibg=#eae9dd guifg=#68607d ctermbg=7 ctermfg=8

hi Special gui=bold guifg=#706a00 term=none cterm=bold ctermfg=11
hi SpecialKey gui=none guibg=#eae9dd guifg=#68607d cterm=none ctermbg=7 ctermfg=8
hi SpecialChar gui=bold guifg=#706a00 cterm=bold ctermfg=11
hi NonText gui=none guibg=#eae9dd guifg=#68607d cterm=none ctermbg=7 ctermfg=8
hi EndOfBuffer gui=bold guifg=#68607d cterm=bold ctermfg=8

hi Directory gui=none guifg=#107410 term=none cterm=none ctermfg=2
hi Question gui=bold guifg=#706a00 cterm=bold ctermfg=11
hi MoreMsg guifg=#4a7240 ctermfg=10
hi ModeMsg gui=bold guifg=#107410 cterm=bold ctermfg=2

hi VimOption guifg=#b63052 ctermfg=5
hi VimGroup guifg=#b63052 ctermfg=5

hi Underlined gui=underline guifg=#464340 cterm=underline ctermfg=0
hi Ignore guibg=#eae9dd guifg=#68607d ctermbg=7 ctermfg=8
hi Conceal guibg=#68607d guifg=#eae9dd ctermbg=8 ctermfg=7

hi SpellBad guibg=#c81000 guifg=#f8f2e5 ctermbg=1 ctermfg=15
hi SpellCap guibg=#806000 guifg=#f8f2e5 ctermbg=3 ctermfg=15
hi SpellRare guibg=#8055aa guifg=#f8f2e5 ctermbg=13 ctermfg=15
hi SpellLocal guibg=#337087 guifg=#f8f2e5 ctermbg=14 ctermfg=15

hi Pmenu gui=italic guibg=#eae9dd guifg=#464340 cterm=none ctermbg=7 ctermfg=0
hi PmenuSel gui=none,bold guibg=#68607d guifg=#f8f2e5 cterm=none,bold ctermbg=8 ctermfg=15
hi PmenuSbar guibg=#eae9dd ctermbg=7
hi PmenuThumb guibg=#68607d ctermbg=8

" Diffs
" -----
hi DiffAdd gui=bold guibg=#107410 guifg=#f8f2e5 cterm=bold ctermbg=10 ctermfg=15
hi DiffDelete gui=none guibg=#c81000 guifg=#f8f2e5 cterm=none ctermbg=9 ctermfg=15
hi DiffChange gui=bold guibg=#eae9dd guifg=#68607d cterm=bold ctermbg=7 ctermfg=8
hi DiffText gui=bold guibg=#eae9dd guifg=#b94000 cterm=bold ctermbg=7 ctermfg=1

hi diffAdded guifg=#107410 ctermfg=2
hi diffRemoved guifg=#c81000 ctermfg=1
hi diffNewFile gui=none guifg=#385dc4 ctermfg=4
hi diffFile gui=none guifg=#806000 cterm=none ctermfg=3

hi GitGutterAdd guibg=#eae9dd guifg=#107410 ctermbg=7 ctermfg=2
hi GitGutterChange gui=bold guibg=#eae9dd guifg=#68607d cterm=bold ctermbg=7 ctermfg=8
hi GitGutterDelete guibg=#eae9dd guifg=#c81000 ctermbg=7 ctermfg=1
hi GitGutterChangeDelete gui=bold guibg=#eae9dd guifg=#c81000 cterm=bold ctermbg=7 ctermfg=1

" Neomake
" -------
hi NeomakeError gui=none guibg=#c81000 guifg=#f8f2e5 cterm=none ctermbg=1 ctermfg=15
hi NeomakeInfo gui=none guibg=#007070 guifg=#f8f2e5 cterm=none ctermbg=6 ctermfg=15
hi NeomakeWarning gui=none guibg=#806000 guifg=#f8f2e5 cterm=none ctermbg=3 ctermfg=15
hi NeomakeMessage gui=none guibg=#706a00 guifg=#f8f2e5 cterm=none ctermbg=11 ctermfg=15

hi NeomakeVirtualtextInfoDefault guifg=#337087 ctermfg=14
hi NeomakeVirtualtextMessageDefault guifg=#706a00 ctermfg=11
hi NeomakeVirtualtextWarningDefault guifg=#806000 ctermfg=3
hi NeomakeVirtualtextErrorDefault guifg=#c81000 ctermfg=1

hi NeomakeStatusGood gui=none guibg=#107410 guifg=#f8f2e5 cterm=none ctermbg=2 ctermfg=15
hi NeomakeStatusGoodNC gui=none guibg=#eae9dd guifg=#107410 cterm=none ctermbg=7 ctermfg=2

hi NeomakeStatColorDefault gui=none guibg=#385dc4 guifg=#f8f2e5 cterm=none ctermbg=4 ctermfg=15
hi NeomakeStatColorTypeE gui=none guibg=#c81000 guifg=#f8f2e5 cterm=none ctermbg=1 ctermfg=15
hi NeomakeStatColorTypeW gui=none guibg=#806000 guifg=#f8f2e5 cterm=none ctermbg=3 ctermfg=15

" Markdown
" --------
hi MarkdownRule gui=bold guibg=#eae9dd guifg=#4a7240 cterm=bold ctermbg=7 ctermfg=10

hi MarkdownHeading gui=bold guifg=#464340 cterm=bold ctermfg=0
hi default link MarkdownH1 MarkdownHeading
hi default link MarkdownH2 MarkdownHeading
hi default link MarkdownH3 MarkdownHeading
hi default link MarkdownH4 MarkdownHeading
hi default link MarkdownH5 MarkdownHeading
hi default link MarkdownH6 MarkdownHeading
hi default link MarkdownHeadingDelimiter MarkdownHeading
hi default link MarkdownHeadingRule MarkdownHeading

hi MarkdownBold gui=bold guifg=#b94000 cterm=bold ctermfg=9
hi default link MarkdownBoldDelimiter MarkdownBold

hi MarkdownItalic gui=italic guifg=#806000 cterm=none ctermfg=3
hi default link MarkdownItalicDelimiter MarkdownItalic

hi MarkdownUrl gui=underline guifg=#385dc4 cterm=underline ctermfg=4
hi MarkdownLinkText gui=none guifg=#0d66c9 cterm=none ctermfg=12
hi MarkdownLinkDelimiter gui=bold guifg=#464340 cterm=bold ctermfg=0
hi default link MarkdownLinkTextDelimiter MarkdownLinkDelimiter

hi MarkdownCode guifg=#b63052 ctermfg=5
hi default link MarkdownCodeDelimiter MarkdownCode

hi MarkdownCodeBlock guifg=#464340 ctermfg=0

hi MarkdownListMarker gui=none guifg=#107410 cterm=none ctermfg=2
hi default link MarkdownOrderedListMarker MarkdownListMarker

" Linting
" -------
hi YcmErrorSection gui=undercurl guisp=#c81000 cterm=underline
hi YcmWarningSection gui=undercurl guisp=#385dc4 cterm=underline
hi SyntasticError gui=undercurl guisp=#c81000 cterm=underline
hi SyntasticWarning gui=undercurl guisp=#385dc4 cterm=underline
hi SyntasticErrorSing guifg=#f8f2e5 guibg=#c81000 ctermfg=15 ctermbg=1
hi SyntasticWarningSign guifg=#f8f2e5 guibg=#385dc4 ctermfg=15 ctermbg=4
