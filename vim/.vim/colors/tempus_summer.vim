" Name: Tempus Summer
" Description: Dark theme with colours inspired by summer evenings by the sea (WCAG AA compliant)
" Author: Protesilaos Stavrou (https://protesilaos.com)
" Meta: Created with the Tempus Themes Generator
" URL: https://gitlab.com/protesilaos/tempus-themes-generator

set background=dark
hi clear
if exists("syntax_on")
	syntax reset
endif
let g:colors_name = "tempus_summer"

" General
" -------
" NOTE the ctermbg=none is for terminals with transparency
hi Normal guibg=#202c3d guifg=#a0abae ctermbg=none ctermfg=15
hi Visual guibg=#a0abae guifg=#202c3d ctermbg=15 ctermfg=0
hi Search gui=underline,bold guibg=#352f49 guifg=#a0abae cterm=underline,bold ctermbg=8 ctermfg=3
hi IncSearch gui=underline,bold guibg=#919ab9 guifg=#202c3d term=none cterm=underline,bold ctermbg=7 ctermfg=0

hi StatusLine gui=none guibg=#a0abae guifg=#202c3d cterm=none ctermbg=15 ctermfg=0
hi StatusLineNC gui=none guibg=#352f49 guifg=#919ab9 cterm=none ctermbg=8 ctermfg=7
hi StatusLineTerm gui=none guibg=#4eac6d guifg=#202c3d cterm=none ctermbg=2 ctermfg=0
hi StatusLineTermNC gui=none guibg=#352f49 guifg=#4eac6d cterm=none ctermbg=8 ctermfg=2

hi VertSplit gui=none cterm=none
hi TabLine gui=none guibg=#352f49 guifg=#919ab9 cterm=none ctermbg=8 ctermfg=7
hi TabLineSel gui=none guibg=#3dab95 guifg=#202c3d cterm=none ctermbg=6 ctermfg=0
hi TabLineFill gui=none cterm=none

hi Comment gui=italic guifg=#919ab9 cterm=none ctermfg=7
hi Todo gui=bold guibg=#352f49 guifg=#bd951a cterm=bold ctermbg=8 ctermfg=11

hi Warning gui=none guibg=#af9a0a guifg=#202c3d cterm=none ctermbg=3 ctermfg=0
hi WarningMsg gui=none guibg=#af9a0a guifg=#202c3d cterm=none ctermbg=3 ctermfg=0
hi Error gui=none guibg=#f76f6e guifg=#202c3d cterm=none ctermbg=1 ctermfg=0
hi ErrorMsg gui=none guibg=#f76f6e guifg=#202c3d cterm=none ctermbg=1 ctermfg=0

hi MatchParen gui=underline,bold guibg=#352f49 guifg=#919ab9 cterm=underline,bold ctermbg=8 ctermfg=7

hi ToolbarLine guibg=#919ab9 guifg=#202c3d term=none ctermbg=7 ctermfg=0
hi ToolbarButton gui=bold guibg=#919ab9 guifg=#202c3d term=none cterm=bold ctermbg=7 ctermfg=0

hi WildMenu guibg=#202c3d guifg=#a0abae term=standout ctermbg=0 ctermfg=15

hi Terminal guibg=#202c3d guifg=#a0abae term=none ctermbg=0 ctermfg=15

" Constructs
" ----------
hi Constant guifg=#609fda ctermfg=4
hi Number guifg=#609fda ctermfg=4
hi Float guifg=#609fda ctermfg=4
hi String guifg=#8196e8 ctermfg=12

hi Function guifg=#cc84ad ctermfg=5
hi Identifier guifg=#c97ed7 term=none ctermfg=13
hi Label guifg=#cc84ad ctermfg=5
hi Tag guifg=#cc84ad ctermfg=5
hi Keyword gui=bold guifg=#c97ed7 gui=bold ctermfg=13

hi Character gui=bold guifg=#2aa9b6 cterm=bold ctermfg=14

hi Type gui=none,bold guifg=#3dab95 term=none cterm=none,bold ctermfg=6
hi Boolean guifg=#3dab95 ctermfg=6
hi StorageClass guifg=#3dab95 ctermfg=6
hi Structure guifg=#3dab95 ctermfg=6
hi Typedef gui=bold guifg=#2aa9b6 cterm=bold ctermfg=14

hi Conditional gui=bold guifg=#4eac6d cterm=bold ctermfg=2
hi Statement gui=none guifg=#57ad47 cterm=none ctermfg=10
hi Repeat gui=bold guifg=#57ad47 cterm=bold ctermfg=10
hi Operator gui=bold guifg=#a0abae cterm=bold ctermfg=15
hi Exception gui=bold guifg=#f76f6e cterm=bold ctermfg=1

hi Preproc gui=none guifg=#eb7b4d term=none cterm=none ctermfg=9
hi PreCondit gui=bold guifg=#eb7b4d cterm=bold ctermfg=9
hi Macro gui=bold guifg=#eb7b4d cterm=bold ctermfg=9
hi Include guifg=#eb7b4d ctermfg=9
hi Define guifg=#eb7b4d ctermfg=9

hi Title gui=bold guibg=#202c3d guifg=#3dab95 cterm=bold ctermbg=0 ctermfg=6

hi Delimeter gui=bold guifg=#cc84ad cterm=bold ctermfg=5
hi Delimiter gui=bold guifg=#cc84ad cterm=bold ctermfg=5
hi SpecialComment gui=bold guifg=#cc84ad cterm=bold ctermfg=5

hi Debug guifg=#c97ed7 ctermfg=13

" Other
" -----
hi LineNr guibg=#352f49 guifg=#919ab9 term=none ctermbg=8 ctermfg=7
hi Cursor guibg=#a0abae guifg=#202c3d ctermbg=15 ctermfg=0
hi CursorLine gui=none guibg=#352f49 term=none cterm=none ctermbg=8
hi CursorColumn gui=none guibg=#352f49 term=none cterm=none ctermbg=8
hi CursorLineNr gui=bold guibg=#919ab9 guifg=#202c3d cterm=bold ctermbg=7 ctermfg=0
hi ColorColumn guibg=#352f49 guifg=#a0abae term=none ctermbg=8 ctermfg=15
hi SignColumn guibg=#352f49 guifg=#919ab9 term=none ctermbg=8 ctermfg=7

hi Folded guibg=#352f49 guifg=#919ab9 ctermbg=8 ctermfg=7
hi FoldColumn guibg=#352f49 guifg=#919ab9 ctermbg=8 ctermfg=7

hi Special gui=bold guifg=#bd951a term=none cterm=bold ctermfg=11
hi SpecialKey gui=none guibg=#352f49 guifg=#919ab9 cterm=none ctermbg=8 ctermfg=7
hi SpecialChar gui=bold guifg=#bd951a cterm=bold ctermfg=11
hi NonText gui=none guibg=#352f49 guifg=#919ab9 cterm=none ctermbg=8 ctermfg=7
hi EndOfBuffer gui=bold guifg=#919ab9 cterm=bold ctermfg=7

hi Directory gui=none guifg=#4eac6d term=none cterm=none ctermfg=2
hi Question gui=bold guifg=#bd951a cterm=bold ctermfg=11
hi MoreMsg guifg=#57ad47 ctermfg=10
hi ModeMsg gui=bold guifg=#4eac6d cterm=bold ctermfg=2

hi VimOption guifg=#cc84ad ctermfg=5
hi VimGroup guifg=#cc84ad ctermfg=5

hi Underlined gui=underline guifg=#a0abae cterm=underline ctermfg=15
hi Ignore guibg=#352f49 guifg=#919ab9 ctermbg=8 ctermfg=7
hi Conceal guibg=#919ab9 guifg=#352f49 ctermbg=7 ctermfg=8

hi SpellBad guibg=#f76f6e guifg=#202c3d ctermbg=1 ctermfg=0
hi SpellCap guibg=#af9a0a guifg=#202c3d ctermbg=3 ctermfg=0
hi SpellRare guibg=#c97ed7 guifg=#202c3d ctermbg=13 ctermfg=0
hi SpellLocal guibg=#2aa9b6 guifg=#202c3d ctermbg=14 ctermfg=0

hi Pmenu gui=italic guibg=#352f49 guifg=#a0abae cterm=none ctermbg=8 ctermfg=15
hi PmenuSel gui=none,bold guibg=#919ab9 guifg=#202c3d cterm=none,bold ctermbg=7 ctermfg=0
hi PmenuSbar guibg=#352f49 ctermbg=8
hi PmenuThumb guibg=#919ab9 ctermbg=7

" Diffs
" -----
hi DiffAdd gui=bold guibg=#4eac6d guifg=#202c3d cterm=bold ctermbg=10 ctermfg=0
hi DiffDelete gui=none guibg=#f76f6e guifg=#202c3d cterm=none ctermbg=9 ctermfg=0
hi DiffChange gui=bold guibg=#352f49 guifg=#919ab9 cterm=bold ctermbg=8 ctermfg=7
hi DiffText gui=bold guibg=#352f49 guifg=#eb7b4d cterm=bold ctermbg=8 ctermfg=1

hi diffAdded guifg=#4eac6d ctermfg=2
hi diffRemoved guifg=#f76f6e ctermfg=1
hi diffNewFile gui=none guifg=#609fda ctermfg=4
hi diffFile gui=none guifg=#af9a0a cterm=none ctermfg=3

hi GitGutterAdd guibg=#352f49 guifg=#4eac6d ctermbg=8 ctermfg=2
hi GitGutterChange gui=bold guibg=#352f49 guifg=#919ab9 cterm=bold ctermbg=8 ctermfg=7
hi GitGutterDelete guibg=#352f49 guifg=#f76f6e ctermbg=8 ctermfg=1
hi GitGutterChangeDelete gui=bold guibg=#352f49 guifg=#f76f6e cterm=bold ctermbg=8 ctermfg=1

" Neomake
" -------
hi NeomakeError gui=none guibg=#f76f6e guifg=#202c3d cterm=none ctermbg=1 ctermfg=0
hi NeomakeInfo gui=none guibg=#3dab95 guifg=#202c3d cterm=none ctermbg=6 ctermfg=0
hi NeomakeWarning gui=none guibg=#af9a0a guifg=#202c3d cterm=none ctermbg=3 ctermfg=0
hi NeomakeMessage gui=none guibg=#bd951a guifg=#202c3d cterm=none ctermbg=11 ctermfg=0

hi NeomakeVirtualtextInfoDefault guifg=#2aa9b6 ctermfg=14
hi NeomakeVirtualtextMessageDefault guifg=#bd951a ctermfg=11
hi NeomakeVirtualtextWarningDefault guifg=#af9a0a ctermfg=3
hi NeomakeVirtualtextErrorDefault guifg=#f76f6e ctermfg=1

hi NeomakeStatusGood gui=none guibg=#4eac6d guifg=#202c3d cterm=none ctermbg=2 ctermfg=0
hi NeomakeStatusGoodNC gui=none guibg=#352f49 guifg=#4eac6d cterm=none ctermbg=8 ctermfg=2

hi NeomakeStatColorDefault gui=none guibg=#609fda guifg=#202c3d cterm=none ctermbg=4 ctermfg=0
hi NeomakeStatColorTypeE gui=none guibg=#f76f6e guifg=#202c3d cterm=none ctermbg=1 ctermfg=0
hi NeomakeStatColorTypeW gui=none guibg=#af9a0a guifg=#202c3d cterm=none ctermbg=3 ctermfg=0

" Markdown
" --------
hi MarkdownRule gui=bold guibg=#352f49 guifg=#57ad47 cterm=bold ctermbg=8 ctermfg=10

hi MarkdownHeading gui=bold guifg=#a0abae cterm=bold ctermfg=15
hi default link MarkdownH1 MarkdownHeading
hi default link MarkdownH2 MarkdownHeading
hi default link MarkdownH3 MarkdownHeading
hi default link MarkdownH4 MarkdownHeading
hi default link MarkdownH5 MarkdownHeading
hi default link MarkdownH6 MarkdownHeading
hi default link MarkdownHeadingDelimiter MarkdownHeading
hi default link MarkdownHeadingRule MarkdownHeading

hi MarkdownBold gui=bold guifg=#eb7b4d cterm=bold ctermfg=9
hi default link MarkdownBoldDelimiter MarkdownBold

hi MarkdownItalic gui=italic guifg=#af9a0a cterm=none ctermfg=3
hi default link MarkdownItalicDelimiter MarkdownItalic

hi MarkdownUrl gui=underline guifg=#609fda cterm=underline ctermfg=4
hi MarkdownLinkText gui=none guifg=#8196e8 cterm=none ctermfg=12
hi MarkdownLinkDelimiter gui=bold guifg=#a0abae cterm=bold ctermfg=15
hi default link MarkdownLinkTextDelimiter MarkdownLinkDelimiter

hi MarkdownCode guifg=#cc84ad ctermfg=5
hi default link MarkdownCodeDelimiter MarkdownCode

hi MarkdownCodeBlock guifg=#a0abae ctermfg=15

hi MarkdownListMarker gui=none guifg=#4eac6d cterm=none ctermfg=2
hi default link MarkdownOrderedListMarker MarkdownListMarker

" Linting
" -------
hi YcmErrorSection gui=undercurl guisp=#f76f6e cterm=underline
hi YcmWarningSection gui=undercurl guisp=#af9a0a cterm=underline
hi SyntasticError gui=undercurl guisp=#f76f6e cterm=underline
hi SyntasticWarning gui=undercurl guisp=#af9a0a cterm=underline
hi SyntasticErrorSing guifg=#202c3d guibg=#f76f6e ctermfg=0 ctermbg=1
hi SyntasticWarningSign guifg=#202c3d guibg=#af9a0a ctermfg=0 ctermbg=3
