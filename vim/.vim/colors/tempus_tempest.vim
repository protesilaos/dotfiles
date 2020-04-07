" Name: Tempus Tempest
" Description: A green-scale, subtle theme for late night hackers (WCAG AAA compliant)
" Author: Protesilaos Stavrou (https://protesilaos.com)
" Meta: Created with the Tempus Themes Generator
" URL: https://gitlab.com/protesilaos/tempus-themes-generator

set background=dark
hi clear
if exists("syntax_on")
	syntax reset
endif
let g:colors_name = "tempus_tempest"

" General
" -------
" NOTE the ctermbg=none is for terminals with transparency
hi Normal guibg=#282b2b guifg=#b6e0ca ctermbg=none ctermfg=15
hi Visual guibg=#b6e0ca guifg=#282b2b ctermbg=15 ctermfg=0
hi Search gui=underline,bold guibg=#303434 guifg=#b6e0ca cterm=underline,bold ctermbg=8 ctermfg=3
hi IncSearch gui=underline,bold guibg=#b0c8ca guifg=#282b2b term=none cterm=underline,bold ctermbg=7 ctermfg=0

hi StatusLine gui=none guibg=#b6e0ca guifg=#282b2b cterm=none ctermbg=15 ctermfg=0
hi StatusLineNC gui=none guibg=#303434 guifg=#b0c8ca cterm=none ctermbg=8 ctermfg=7
hi StatusLineTerm gui=none guibg=#7ad67a guifg=#282b2b cterm=none ctermbg=2 ctermfg=0
hi StatusLineTermNC gui=none guibg=#303434 guifg=#7ad67a cterm=none ctermbg=8 ctermfg=2

hi VertSplit gui=none cterm=none
hi TabLine gui=none guibg=#303434 guifg=#b0c8ca cterm=none ctermbg=8 ctermfg=7
hi TabLineSel gui=none guibg=#8ad0b0 guifg=#282b2b cterm=none ctermbg=6 ctermfg=0
hi TabLineFill gui=none cterm=none

hi Comment gui=italic guifg=#b0c8ca cterm=none ctermfg=7
hi Todo gui=bold guibg=#303434 guifg=#bbde4f cterm=bold ctermbg=8 ctermfg=11

hi Warning gui=none guibg=#bfc94a guifg=#282b2b cterm=none ctermbg=3 ctermfg=0
hi WarningMsg gui=none guibg=#bfc94a guifg=#282b2b cterm=none ctermbg=3 ctermfg=0
hi Error gui=none guibg=#c6c80a guifg=#282b2b cterm=none ctermbg=1 ctermfg=0
hi ErrorMsg gui=none guibg=#c6c80a guifg=#282b2b cterm=none ctermbg=1 ctermfg=0

hi MatchParen gui=underline,bold guibg=#303434 guifg=#b0c8ca cterm=underline,bold ctermbg=8 ctermfg=7

hi ToolbarLine guibg=#b0c8ca guifg=#282b2b term=none ctermbg=7 ctermfg=0
hi ToolbarButton gui=bold guibg=#b0c8ca guifg=#282b2b term=none cterm=bold ctermbg=7 ctermfg=0

hi WildMenu guibg=#282b2b guifg=#b6e0ca term=standout ctermbg=0 ctermfg=15

hi Terminal guibg=#282b2b guifg=#b6e0ca term=none ctermbg=0 ctermfg=15

" Constructs
" ----------
hi Constant guifg=#60d4cd ctermfg=4
hi Number guifg=#60d4cd ctermfg=4
hi Float guifg=#60d4cd ctermfg=4
hi String guifg=#74e4cd ctermfg=12

hi Function guifg=#c0c4aa ctermfg=5
hi Identifier guifg=#d2d4aa term=none ctermfg=13
hi Label guifg=#c0c4aa ctermfg=5
hi Tag guifg=#c0c4aa ctermfg=5
hi Keyword gui=bold guifg=#d2d4aa gui=bold ctermfg=13

hi Character gui=bold guifg=#9bdfc4 cterm=bold ctermfg=14

hi Type gui=none,bold guifg=#8ad0b0 term=none cterm=none,bold ctermfg=6
hi Boolean guifg=#8ad0b0 ctermfg=6
hi StorageClass guifg=#8ad0b0 ctermfg=6
hi Structure guifg=#8ad0b0 ctermfg=6
hi Typedef gui=bold guifg=#9bdfc4 cterm=bold ctermfg=14

hi Conditional gui=bold guifg=#7ad67a cterm=bold ctermfg=2
hi Statement gui=none guifg=#99e299 cterm=none ctermfg=10
hi Repeat gui=bold guifg=#99e299 cterm=bold ctermfg=10
hi Operator gui=bold guifg=#b6e0ca cterm=bold ctermfg=15
hi Exception gui=bold guifg=#c6c80a cterm=bold ctermfg=1

hi Preproc gui=none guifg=#d1d933 term=none cterm=none ctermfg=9
hi PreCondit gui=bold guifg=#d1d933 cterm=bold ctermfg=9
hi Macro gui=bold guifg=#d1d933 cterm=bold ctermfg=9
hi Include guifg=#d1d933 ctermfg=9
hi Define guifg=#d1d933 ctermfg=9

hi Title gui=bold guibg=#282b2b guifg=#8ad0b0 cterm=bold ctermbg=0 ctermfg=6

hi Delimeter gui=bold guifg=#c0c4aa cterm=bold ctermfg=5
hi Delimiter gui=bold guifg=#c0c4aa cterm=bold ctermfg=5
hi SpecialComment gui=bold guifg=#c0c4aa cterm=bold ctermfg=5

hi Debug guifg=#d2d4aa ctermfg=13

" Other
" -----
hi LineNr guibg=#303434 guifg=#b0c8ca term=none ctermbg=8 ctermfg=7
hi Cursor guibg=#b6e0ca guifg=#282b2b ctermbg=15 ctermfg=0
hi CursorLine gui=none guibg=#303434 term=none cterm=none ctermbg=8
hi CursorColumn gui=none guibg=#303434 term=none cterm=none ctermbg=8
hi CursorLineNr gui=bold guibg=#b0c8ca guifg=#282b2b cterm=bold ctermbg=7 ctermfg=0
hi ColorColumn guibg=#303434 guifg=#b6e0ca term=none ctermbg=8 ctermfg=15
hi SignColumn guibg=#303434 guifg=#b0c8ca term=none ctermbg=8 ctermfg=7

hi Folded guibg=#303434 guifg=#b0c8ca ctermbg=8 ctermfg=7
hi FoldColumn guibg=#303434 guifg=#b0c8ca ctermbg=8 ctermfg=7

hi Special gui=bold guifg=#bbde4f term=none cterm=bold ctermfg=11
hi SpecialKey gui=none guibg=#303434 guifg=#b0c8ca cterm=none ctermbg=8 ctermfg=7
hi SpecialChar gui=bold guifg=#bbde4f cterm=bold ctermfg=11
hi NonText gui=none guibg=#303434 guifg=#b0c8ca cterm=none ctermbg=8 ctermfg=7
hi EndOfBuffer gui=bold guifg=#b0c8ca cterm=bold ctermfg=7

hi Directory gui=none guifg=#7ad67a term=none cterm=none ctermfg=2
hi Question gui=bold guifg=#bbde4f cterm=bold ctermfg=11
hi MoreMsg guifg=#99e299 ctermfg=10
hi ModeMsg gui=bold guifg=#7ad67a cterm=bold ctermfg=2

hi VimOption guifg=#c0c4aa ctermfg=5
hi VimGroup guifg=#c0c4aa ctermfg=5

hi Underlined gui=underline guifg=#b6e0ca cterm=underline ctermfg=15
hi Ignore guibg=#303434 guifg=#b0c8ca ctermbg=8 ctermfg=7
hi Conceal guibg=#b0c8ca guifg=#303434 ctermbg=7 ctermfg=8

hi SpellBad guibg=#c6c80a guifg=#282b2b ctermbg=1 ctermfg=0
hi SpellCap guibg=#bfc94a guifg=#282b2b ctermbg=3 ctermfg=0
hi SpellRare guibg=#d2d4aa guifg=#282b2b ctermbg=13 ctermfg=0
hi SpellLocal guibg=#9bdfc4 guifg=#282b2b ctermbg=14 ctermfg=0

hi Pmenu gui=italic guibg=#303434 guifg=#b6e0ca cterm=none ctermbg=8 ctermfg=15
hi PmenuSel gui=none,bold guibg=#b0c8ca guifg=#282b2b cterm=none,bold ctermbg=7 ctermfg=0
hi PmenuSbar guibg=#303434 ctermbg=8
hi PmenuThumb guibg=#b0c8ca ctermbg=7

" Diffs
" -----
hi DiffAdd gui=bold guibg=#7ad67a guifg=#282b2b cterm=bold ctermbg=10 ctermfg=0
hi DiffDelete gui=none guibg=#c6c80a guifg=#282b2b cterm=none ctermbg=9 ctermfg=0
hi DiffChange gui=bold guibg=#303434 guifg=#b0c8ca cterm=bold ctermbg=8 ctermfg=7
hi DiffText gui=bold guibg=#303434 guifg=#d1d933 cterm=bold ctermbg=8 ctermfg=1

hi diffAdded guifg=#7ad67a ctermfg=2
hi diffRemoved guifg=#c6c80a ctermfg=1
hi diffNewFile gui=none guifg=#60d4cd ctermfg=4
hi diffFile gui=none guifg=#bfc94a cterm=none ctermfg=3

hi GitGutterAdd guibg=#303434 guifg=#7ad67a ctermbg=8 ctermfg=2
hi GitGutterChange gui=bold guibg=#303434 guifg=#b0c8ca cterm=bold ctermbg=8 ctermfg=7
hi GitGutterDelete guibg=#303434 guifg=#c6c80a ctermbg=8 ctermfg=1
hi GitGutterChangeDelete gui=bold guibg=#303434 guifg=#c6c80a cterm=bold ctermbg=8 ctermfg=1

" Neomake
" -------
hi NeomakeError gui=none guibg=#c6c80a guifg=#282b2b cterm=none ctermbg=1 ctermfg=0
hi NeomakeInfo gui=none guibg=#8ad0b0 guifg=#282b2b cterm=none ctermbg=6 ctermfg=0
hi NeomakeWarning gui=none guibg=#bfc94a guifg=#282b2b cterm=none ctermbg=3 ctermfg=0
hi NeomakeMessage gui=none guibg=#bbde4f guifg=#282b2b cterm=none ctermbg=11 ctermfg=0

hi NeomakeVirtualtextInfoDefault guifg=#9bdfc4 ctermfg=14
hi NeomakeVirtualtextMessageDefault guifg=#bbde4f ctermfg=11
hi NeomakeVirtualtextWarningDefault guifg=#bfc94a ctermfg=3
hi NeomakeVirtualtextErrorDefault guifg=#c6c80a ctermfg=1

hi NeomakeStatusGood gui=none guibg=#7ad67a guifg=#282b2b cterm=none ctermbg=2 ctermfg=0
hi NeomakeStatusGoodNC gui=none guibg=#303434 guifg=#7ad67a cterm=none ctermbg=8 ctermfg=2

hi NeomakeStatColorDefault gui=none guibg=#60d4cd guifg=#282b2b cterm=none ctermbg=4 ctermfg=0
hi NeomakeStatColorTypeE gui=none guibg=#c6c80a guifg=#282b2b cterm=none ctermbg=1 ctermfg=0
hi NeomakeStatColorTypeW gui=none guibg=#bfc94a guifg=#282b2b cterm=none ctermbg=3 ctermfg=0

" Markdown
" --------
hi MarkdownRule gui=bold guibg=#303434 guifg=#99e299 cterm=bold ctermbg=8 ctermfg=10

hi MarkdownHeading gui=bold guifg=#b6e0ca cterm=bold ctermfg=15
hi default link MarkdownH1 MarkdownHeading
hi default link MarkdownH2 MarkdownHeading
hi default link MarkdownH3 MarkdownHeading
hi default link MarkdownH4 MarkdownHeading
hi default link MarkdownH5 MarkdownHeading
hi default link MarkdownH6 MarkdownHeading
hi default link MarkdownHeadingDelimiter MarkdownHeading
hi default link MarkdownHeadingRule MarkdownHeading

hi MarkdownBold gui=bold guifg=#d1d933 cterm=bold ctermfg=9
hi default link MarkdownBoldDelimiter MarkdownBold

hi MarkdownItalic gui=italic guifg=#bfc94a cterm=none ctermfg=3
hi default link MarkdownItalicDelimiter MarkdownItalic

hi MarkdownUrl gui=underline guifg=#60d4cd cterm=underline ctermfg=4
hi MarkdownLinkText gui=none guifg=#74e4cd cterm=none ctermfg=12
hi MarkdownLinkDelimiter gui=bold guifg=#b6e0ca cterm=bold ctermfg=15
hi default link MarkdownLinkTextDelimiter MarkdownLinkDelimiter

hi MarkdownCode guifg=#c0c4aa ctermfg=5
hi default link MarkdownCodeDelimiter MarkdownCode

hi MarkdownCodeBlock guifg=#b6e0ca ctermfg=15

hi MarkdownListMarker gui=none guifg=#7ad67a cterm=none ctermfg=2
hi default link MarkdownOrderedListMarker MarkdownListMarker

" Linting
" -------
hi YcmErrorSection gui=undercurl guisp=#c6c80a cterm=underline
hi YcmWarningSection gui=undercurl guisp=#bfc94a cterm=underline
hi SyntasticError gui=undercurl guisp=#c6c80a cterm=underline
hi SyntasticWarning gui=undercurl guisp=#bfc94a cterm=underline
hi SyntasticErrorSing guifg=#282b2b guibg=#c6c80a ctermfg=0 ctermbg=1
hi SyntasticWarningSign guifg=#282b2b guibg=#bfc94a ctermfg=0 ctermbg=3
