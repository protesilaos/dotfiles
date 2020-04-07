" Name: Tempus Past
" Description: Light theme inspired by old vaporwave concept art (WCAG AA compliant)
" Author: Protesilaos Stavrou (https://protesilaos.com)
" Meta: Created with the Tempus Themes Generator
" URL: https://gitlab.com/protesilaos/tempus-themes-generator

set background=light
hi clear
if exists("syntax_on")
	syntax reset
endif
let g:colors_name = "tempus_past"

" General
" -------
" NOTE the ctermbg=none is for terminals with transparency
hi Normal guibg=#f3f2f4 guifg=#53545b ctermbg=none ctermfg=0
hi Visual guibg=#53545b guifg=#f3f2f4 ctermbg=0 ctermfg=15
hi Search gui=underline,bold guibg=#ece6de guifg=#53545b cterm=underline,bold ctermbg=7 ctermfg=3
hi IncSearch gui=underline,bold guibg=#80565d guifg=#f3f2f4 term=none cterm=underline,bold ctermbg=8 ctermfg=15

hi StatusLine gui=none guibg=#53545b guifg=#f3f2f4 cterm=none ctermbg=0 ctermfg=15
hi StatusLineNC gui=none guibg=#ece6de guifg=#80565d cterm=none ctermbg=7 ctermfg=8
hi StatusLineTerm gui=none guibg=#0a7040 guifg=#f3f2f4 cterm=none ctermbg=2 ctermfg=15
hi StatusLineTermNC gui=none guibg=#ece6de guifg=#0a7040 cterm=none ctermbg=7 ctermfg=2

hi VertSplit gui=none cterm=none
hi TabLine gui=none guibg=#ece6de guifg=#80565d cterm=none ctermbg=7 ctermfg=8
hi TabLineSel gui=none guibg=#096a83 guifg=#f3f2f4 cterm=none ctermbg=6 ctermfg=15
hi TabLineFill gui=none cterm=none

hi Comment gui=italic guifg=#80565d cterm=none ctermfg=8
hi Todo gui=bold guibg=#ece6de guifg=#9d524a cterm=bold ctermbg=7 ctermfg=11

hi Warning gui=none guibg=#a6403a guifg=#f3f2f4 cterm=none ctermbg=3 ctermfg=15
hi WarningMsg gui=none guibg=#a6403a guifg=#f3f2f4 cterm=none ctermbg=3 ctermfg=15
hi Error gui=none guibg=#c00c50 guifg=#f3f2f4 cterm=none ctermbg=1 ctermfg=15
hi ErrorMsg gui=none guibg=#c00c50 guifg=#f3f2f4 cterm=none ctermbg=1 ctermfg=15

hi MatchParen gui=underline,bold guibg=#ece6de guifg=#80565d cterm=underline,bold ctermbg=7 ctermfg=8

hi ToolbarLine guibg=#80565d guifg=#f3f2f4 term=none ctermbg=8 ctermfg=15
hi ToolbarButton gui=bold guibg=#80565d guifg=#f3f2f4 term=none cterm=bold ctermbg=8 ctermfg=15

hi WildMenu guibg=#f3f2f4 guifg=#53545b term=standout ctermbg=15 ctermfg=0

hi Terminal guibg=#f3f2f4 guifg=#53545b term=none ctermbg=15 ctermfg=0

" Constructs
" ----------
hi Constant guifg=#1763aa ctermfg=4
hi Number guifg=#1763aa ctermfg=4
hi Float guifg=#1763aa ctermfg=4
hi String guifg=#5a5ebb ctermfg=12

hi Function guifg=#b02874 ctermfg=5
hi Identifier guifg=#b225ab term=none ctermfg=13
hi Label guifg=#b02874 ctermfg=5
hi Tag guifg=#b02874 ctermfg=5
hi Keyword gui=bold guifg=#b225ab gui=bold ctermfg=13

hi Character gui=bold guifg=#07737a cterm=bold ctermfg=14

hi Type gui=none,bold guifg=#096a83 term=none cterm=none,bold ctermfg=6
hi Boolean guifg=#096a83 ctermfg=6
hi StorageClass guifg=#096a83 ctermfg=6
hi Structure guifg=#096a83 ctermfg=6
hi Typedef gui=bold guifg=#07737a cterm=bold ctermfg=14

hi Conditional gui=bold guifg=#0a7040 cterm=bold ctermfg=2
hi Statement gui=none guifg=#407343 cterm=none ctermfg=10
hi Repeat gui=bold guifg=#407343 cterm=bold ctermfg=10
hi Operator gui=bold guifg=#53545b cterm=bold ctermfg=0
hi Exception gui=bold guifg=#c00c50 cterm=bold ctermfg=1

hi Preproc gui=none guifg=#bd3636 term=none cterm=none ctermfg=9
hi PreCondit gui=bold guifg=#bd3636 cterm=bold ctermfg=9
hi Macro gui=bold guifg=#bd3636 cterm=bold ctermfg=9
hi Include guifg=#bd3636 ctermfg=9
hi Define guifg=#bd3636 ctermfg=9

hi Title gui=bold guibg=#f3f2f4 guifg=#096a83 cterm=bold ctermbg=15 ctermfg=6

hi Delimeter gui=bold guifg=#b02874 cterm=bold ctermfg=5
hi Delimiter gui=bold guifg=#b02874 cterm=bold ctermfg=5
hi SpecialComment gui=bold guifg=#b02874 cterm=bold ctermfg=5

hi Debug guifg=#b225ab ctermfg=13

" Other
" -----
hi LineNr guibg=#ece6de guifg=#80565d term=none ctermbg=7 ctermfg=8
hi Cursor guibg=#53545b guifg=#f3f2f4 ctermbg=0 ctermfg=15
hi CursorLine gui=none guibg=#ece6de term=none cterm=none ctermbg=7
hi CursorColumn gui=none guibg=#ece6de term=none cterm=none ctermbg=7
hi CursorLineNr gui=bold guibg=#80565d guifg=#f3f2f4 cterm=bold ctermbg=8 ctermfg=15
hi ColorColumn guibg=#ece6de guifg=#53545b term=none ctermbg=7 ctermfg=0
hi SignColumn guibg=#ece6de guifg=#80565d term=none ctermbg=7 ctermfg=8

hi Folded guibg=#ece6de guifg=#80565d ctermbg=7 ctermfg=8
hi FoldColumn guibg=#ece6de guifg=#80565d ctermbg=7 ctermfg=8

hi Special gui=bold guifg=#9d524a term=none cterm=bold ctermfg=11
hi SpecialKey gui=none guibg=#ece6de guifg=#80565d cterm=none ctermbg=7 ctermfg=8
hi SpecialChar gui=bold guifg=#9d524a cterm=bold ctermfg=11
hi NonText gui=none guibg=#ece6de guifg=#80565d cterm=none ctermbg=7 ctermfg=8
hi EndOfBuffer gui=bold guifg=#80565d cterm=bold ctermfg=8

hi Directory gui=none guifg=#0a7040 term=none cterm=none ctermfg=2
hi Question gui=bold guifg=#9d524a cterm=bold ctermfg=11
hi MoreMsg guifg=#407343 ctermfg=10
hi ModeMsg gui=bold guifg=#0a7040 cterm=bold ctermfg=2

hi VimOption guifg=#b02874 ctermfg=5
hi VimGroup guifg=#b02874 ctermfg=5

hi Underlined gui=underline guifg=#53545b cterm=underline ctermfg=0
hi Ignore guibg=#ece6de guifg=#80565d ctermbg=7 ctermfg=8
hi Conceal guibg=#80565d guifg=#ece6de ctermbg=8 ctermfg=7

hi SpellBad guibg=#c00c50 guifg=#f3f2f4 ctermbg=1 ctermfg=15
hi SpellCap guibg=#a6403a guifg=#f3f2f4 ctermbg=3 ctermfg=15
hi SpellRare guibg=#b225ab guifg=#f3f2f4 ctermbg=13 ctermfg=15
hi SpellLocal guibg=#07737a guifg=#f3f2f4 ctermbg=14 ctermfg=15

hi Pmenu gui=italic guibg=#ece6de guifg=#53545b cterm=none ctermbg=7 ctermfg=0
hi PmenuSel gui=none,bold guibg=#80565d guifg=#f3f2f4 cterm=none,bold ctermbg=8 ctermfg=15
hi PmenuSbar guibg=#ece6de ctermbg=7
hi PmenuThumb guibg=#80565d ctermbg=8

" Diffs
" -----
hi DiffAdd gui=bold guibg=#0a7040 guifg=#f3f2f4 cterm=bold ctermbg=10 ctermfg=15
hi DiffDelete gui=none guibg=#c00c50 guifg=#f3f2f4 cterm=none ctermbg=9 ctermfg=15
hi DiffChange gui=bold guibg=#ece6de guifg=#80565d cterm=bold ctermbg=7 ctermfg=8
hi DiffText gui=bold guibg=#ece6de guifg=#bd3636 cterm=bold ctermbg=7 ctermfg=1

hi diffAdded guifg=#0a7040 ctermfg=2
hi diffRemoved guifg=#c00c50 ctermfg=1
hi diffNewFile gui=none guifg=#1763aa ctermfg=4
hi diffFile gui=none guifg=#a6403a cterm=none ctermfg=3

hi GitGutterAdd guibg=#ece6de guifg=#0a7040 ctermbg=7 ctermfg=2
hi GitGutterChange gui=bold guibg=#ece6de guifg=#80565d cterm=bold ctermbg=7 ctermfg=8
hi GitGutterDelete guibg=#ece6de guifg=#c00c50 ctermbg=7 ctermfg=1
hi GitGutterChangeDelete gui=bold guibg=#ece6de guifg=#c00c50 cterm=bold ctermbg=7 ctermfg=1

" Neomake
" -------
hi NeomakeError gui=none guibg=#c00c50 guifg=#f3f2f4 cterm=none ctermbg=1 ctermfg=15
hi NeomakeInfo gui=none guibg=#096a83 guifg=#f3f2f4 cterm=none ctermbg=6 ctermfg=15
hi NeomakeWarning gui=none guibg=#a6403a guifg=#f3f2f4 cterm=none ctermbg=3 ctermfg=15
hi NeomakeMessage gui=none guibg=#9d524a guifg=#f3f2f4 cterm=none ctermbg=11 ctermfg=15

hi NeomakeVirtualtextInfoDefault guifg=#07737a ctermfg=14
hi NeomakeVirtualtextMessageDefault guifg=#9d524a ctermfg=11
hi NeomakeVirtualtextWarningDefault guifg=#a6403a ctermfg=3
hi NeomakeVirtualtextErrorDefault guifg=#c00c50 ctermfg=1

hi NeomakeStatusGood gui=none guibg=#0a7040 guifg=#f3f2f4 cterm=none ctermbg=2 ctermfg=15
hi NeomakeStatusGoodNC gui=none guibg=#ece6de guifg=#0a7040 cterm=none ctermbg=7 ctermfg=2

hi NeomakeStatColorDefault gui=none guibg=#1763aa guifg=#f3f2f4 cterm=none ctermbg=4 ctermfg=15
hi NeomakeStatColorTypeE gui=none guibg=#c00c50 guifg=#f3f2f4 cterm=none ctermbg=1 ctermfg=15
hi NeomakeStatColorTypeW gui=none guibg=#a6403a guifg=#f3f2f4 cterm=none ctermbg=3 ctermfg=15

" Markdown
" --------
hi MarkdownRule gui=bold guibg=#ece6de guifg=#407343 cterm=bold ctermbg=7 ctermfg=10

hi MarkdownHeading gui=bold guifg=#53545b cterm=bold ctermfg=0
hi default link MarkdownH1 MarkdownHeading
hi default link MarkdownH2 MarkdownHeading
hi default link MarkdownH3 MarkdownHeading
hi default link MarkdownH4 MarkdownHeading
hi default link MarkdownH5 MarkdownHeading
hi default link MarkdownH6 MarkdownHeading
hi default link MarkdownHeadingDelimiter MarkdownHeading
hi default link MarkdownHeadingRule MarkdownHeading

hi MarkdownBold gui=bold guifg=#bd3636 cterm=bold ctermfg=9
hi default link MarkdownBoldDelimiter MarkdownBold

hi MarkdownItalic gui=italic guifg=#a6403a cterm=none ctermfg=3
hi default link MarkdownItalicDelimiter MarkdownItalic

hi MarkdownUrl gui=underline guifg=#1763aa cterm=underline ctermfg=4
hi MarkdownLinkText gui=none guifg=#5a5ebb cterm=none ctermfg=12
hi MarkdownLinkDelimiter gui=bold guifg=#53545b cterm=bold ctermfg=0
hi default link MarkdownLinkTextDelimiter MarkdownLinkDelimiter

hi MarkdownCode guifg=#b02874 ctermfg=5
hi default link MarkdownCodeDelimiter MarkdownCode

hi MarkdownCodeBlock guifg=#53545b ctermfg=0

hi MarkdownListMarker gui=none guifg=#0a7040 cterm=none ctermfg=2
hi default link MarkdownOrderedListMarker MarkdownListMarker

" Linting
" -------
hi YcmErrorSection gui=undercurl guisp=#c00c50 cterm=underline
hi YcmWarningSection gui=undercurl guisp=#1763aa cterm=underline
hi SyntasticError gui=undercurl guisp=#c00c50 cterm=underline
hi SyntasticWarning gui=undercurl guisp=#1763aa cterm=underline
hi SyntasticErrorSing guifg=#f3f2f4 guibg=#c00c50 ctermfg=15 ctermbg=1
hi SyntasticWarningSign guifg=#f3f2f4 guibg=#1763aa ctermfg=15 ctermbg=4
