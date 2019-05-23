" Name: Tempus Dawn
" Description: Light theme with a soft, slightly desaturated palette (WCAG AA compliant)
" Author: Protesilaos Stavrou (https://protesilaos.com)
" Meta: Created with the Tempus Themes Generator
" URL: https://gitlab.com/protesilaos/tempus-themes-generator

set background=light
hi clear
if exists("syntax_on")
	syntax reset
endif
let g:colors_name = "tempus_dawn"

" General
" -------
" NOTE the ctermbg=none is for terminals with transparency
hi Normal guibg=#f3f4f6 guifg=#525470 ctermbg=none ctermfg=0
hi Visual guibg=#525470 guifg=#f3f4f6 ctermbg=0 ctermfg=15
hi Search gui=underline,bold guibg=#e6e7ea guifg=#525470 cterm=underline,bold ctermbg=7 ctermfg=3
hi IncSearch gui=underline,bold guibg=#705e4d guifg=#f3f4f6 term=none cterm=underline,bold ctermbg=8 ctermfg=15

hi StatusLine gui=none guibg=#525470 guifg=#f3f4f6 cterm=none ctermbg=0 ctermfg=15
hi StatusLineNC gui=none guibg=#e6e7ea guifg=#705e4d cterm=none ctermbg=7 ctermfg=8
hi StatusLineTerm gui=none guibg=#306130 guifg=#f3f4f6 cterm=none ctermbg=2 ctermfg=15
hi StatusLineTermNC gui=none guibg=#e6e7ea guifg=#306130 cterm=none ctermbg=7 ctermfg=2

hi VertSplit gui=none cterm=none
hi TabLine gui=none guibg=#e6e7ea guifg=#705e4d cterm=none ctermbg=7 ctermfg=8
hi TabLineSel gui=none guibg=#186060 guifg=#f3f4f6 cterm=none ctermbg=6 ctermfg=15
hi TabLineFill gui=none cterm=none

hi Comment gui=italic guifg=#705e4d cterm=none ctermfg=8
hi Todo gui=bold guibg=#e6e7ea guifg=#8e5319 cterm=bold ctermbg=7 ctermfg=11

hi Warning gui=none guibg=#73500a guifg=#f3f4f6 cterm=none ctermbg=3 ctermfg=15
hi WarningMsg gui=none guibg=#73500a guifg=#f3f4f6 cterm=none ctermbg=3 ctermfg=15
hi Error gui=none guibg=#9b3132 guifg=#f3f4f6 cterm=none ctermbg=1 ctermfg=15
hi ErrorMsg gui=none guibg=#9b3132 guifg=#f3f4f6 cterm=none ctermbg=1 ctermfg=15

hi MatchParen gui=underline,bold guibg=#e6e7ea guifg=#705e4d cterm=underline,bold ctermbg=7 ctermfg=8

hi ToolbarLine guibg=#705e4d guifg=#f3f4f6 term=none ctermbg=8 ctermfg=15
hi ToolbarButton gui=bold guibg=#705e4d guifg=#f3f4f6 term=none cterm=bold ctermbg=8 ctermfg=15

hi WildMenu guibg=#f3f4f6 guifg=#525470 term=standout ctermbg=15 ctermfg=0

hi Terminal guibg=#f3f4f6 guifg=#525470 term=none ctermbg=15 ctermfg=0

" Constructs
" ----------
hi Constant guifg=#4c547e ctermfg=4
hi Number guifg=#4c547e ctermfg=4
hi Float guifg=#4c547e ctermfg=4
hi String guifg=#5c5d8c ctermfg=12

hi Function guifg=#883c64 ctermfg=5
hi Identifier guifg=#a24055 term=none ctermfg=13
hi Label guifg=#883c64 ctermfg=5
hi Tag guifg=#883c64 ctermfg=5
hi Keyword gui=bold guifg=#a24055 gui=bold ctermfg=13

hi Character gui=bold guifg=#2d6978 cterm=bold ctermfg=14

hi Type gui=none,bold guifg=#186060 term=none cterm=none,bold ctermfg=6
hi Boolean guifg=#186060 ctermfg=6
hi StorageClass guifg=#186060 ctermfg=6
hi Structure guifg=#186060 ctermfg=6
hi Typedef gui=bold guifg=#2d6978 cterm=bold ctermfg=14

hi Conditional gui=bold guifg=#306130 cterm=bold ctermfg=2
hi Statement gui=none guifg=#4e6938 cterm=none ctermfg=10
hi Repeat gui=bold guifg=#4e6938 cterm=bold ctermfg=10
hi Operator gui=bold guifg=#525470 cterm=bold ctermfg=0
hi Exception gui=bold guifg=#9b3132 cterm=bold ctermfg=1

hi Preproc gui=none guifg=#9b474d term=none cterm=none ctermfg=9
hi PreCondit gui=bold guifg=#9b474d cterm=bold ctermfg=9
hi Macro gui=bold guifg=#9b474d cterm=bold ctermfg=9
hi Include guifg=#9b474d ctermfg=9
hi Define guifg=#9b474d ctermfg=9

hi Title gui=bold guibg=#f3f4f6 guifg=#186060 cterm=bold ctermbg=15 ctermfg=6

hi Delimeter gui=bold guifg=#883c64 cterm=bold ctermfg=5
hi Delimiter gui=bold guifg=#883c64 cterm=bold ctermfg=5
hi SpecialComment gui=bold guifg=#883c64 cterm=bold ctermfg=5

hi Debug guifg=#a24055 ctermfg=13

" Other
" -----
hi LineNr guibg=#e6e7ea guifg=#705e4d term=none ctermbg=7 ctermfg=8
hi Cursor guibg=#525470 guifg=#f3f4f6 ctermbg=0 ctermfg=15
hi CursorLine gui=none guibg=NONE term=none cterm=none ctermbg=none
hi CursorColumn gui=none guibg=#e6e7ea term=none cterm=none ctermbg=7
hi CursorLineNr gui=bold guibg=#705e4d guifg=#f3f4f6 cterm=bold ctermbg=8 ctermfg=15
hi ColorColumn guibg=#e6e7ea guifg=#525470 term=none ctermbg=7 ctermfg=0

hi Folded guibg=#e6e7ea guifg=#705e4d ctermbg=7 ctermfg=8
hi FoldColumn guibg=#e6e7ea guifg=#705e4d ctermbg=7 ctermfg=8

hi Special gui=bold guifg=#8e5319 term=none cterm=bold ctermfg=11
hi SpecialKey gui=none guibg=#e6e7ea guifg=#705e4d cterm=none ctermbg=7 ctermfg=8
hi SpecialChar gui=bold guifg=#8e5319 cterm=bold ctermfg=11
hi NonText gui=none guibg=#e6e7ea guifg=#705e4d cterm=none ctermbg=7 ctermfg=8
hi EndOfBuffer gui=bold guifg=#705e4d cterm=bold ctermfg=8

hi Directory gui=none guifg=#306130 term=none cterm=none ctermfg=2
hi Question gui=bold guifg=#8e5319 cterm=bold ctermfg=11
hi MoreMsg guifg=#4e6938 ctermfg=10
hi ModeMsg gui=bold guifg=#306130 cterm=bold ctermfg=2

hi VimOption guifg=#883c64 ctermfg=5
hi VimGroup guifg=#883c64 ctermfg=5

hi Underlined gui=underline guifg=#525470 cterm=underline ctermfg=0
hi Ignore guibg=#e6e7ea guifg=#705e4d ctermbg=7 ctermfg=8
hi Conceal guibg=#705e4d guifg=#e6e7ea ctermbg=8 ctermfg=7

hi SpellBad guibg=#9b3132 guifg=#f3f4f6 ctermbg=1 ctermfg=15
hi SpellCap guibg=#73500a guifg=#f3f4f6 ctermbg=3 ctermfg=15
hi SpellRare guibg=#a24055 guifg=#f3f4f6 ctermbg=13 ctermfg=15
hi SpellLocal guibg=#2d6978 guifg=#f3f4f6 ctermbg=14 ctermfg=15

hi Pmenu gui=italic guibg=#e6e7ea guifg=#525470 cterm=none ctermbg=7 ctermfg=0
hi PmenuSel gui=none,bold guibg=#705e4d guifg=#f3f4f6 cterm=none,bold ctermbg=8 ctermfg=15
hi PmenuSbar guibg=#e6e7ea ctermbg=7
hi PmenuThumb guibg=#705e4d ctermbg=8

" Diffs
" -----
hi DiffAdd gui=bold guibg=#306130 guifg=#f3f4f6 cterm=bold ctermbg=10 ctermfg=15
hi DiffDelete gui=none guibg=#9b3132 guifg=#f3f4f6 cterm=none ctermbg=9 ctermfg=15
hi DiffChange gui=bold guibg=#e6e7ea guifg=#705e4d cterm=bold ctermbg=7 ctermfg=8
hi DiffText gui=bold guibg=#e6e7ea guifg=#9b474d cterm=bold ctermbg=7 ctermfg=1

hi diffAdded guifg=#306130 ctermfg=2
hi diffRemoved guifg=#9b3132 ctermfg=1
hi diffNewFile gui=none guifg=#4c547e ctermfg=4
hi diffFile gui=none guifg=#73500a cterm=none ctermfg=3

" Markdown
" --------
hi MarkdownRule gui=bold guibg=#e6e7ea guifg=#4e6938 cterm=bold ctermbg=7 ctermfg=10

hi MarkdownHeading gui=bold guifg=#525470 cterm=bold ctermfg=0
hi default link MarkdownH1 MarkdownHeading
hi default link MarkdownH2 MarkdownHeading
hi default link MarkdownH3 MarkdownHeading
hi default link MarkdownH4 MarkdownHeading
hi default link MarkdownH5 MarkdownHeading
hi default link MarkdownH6 MarkdownHeading
hi default link MarkdownHeadingDelimiter MarkdownHeading
hi default link MarkdownHeadingRule MarkdownHeading

hi MarkdownBold gui=bold guifg=#9b474d cterm=bold ctermfg=9
hi default link MarkdownBoldDelimiter MarkdownBold

hi MarkdownItalic gui=italic guifg=#73500a cterm=none ctermfg=3
hi default link MarkdownItalicDelimiter MarkdownItalic

hi MarkdownUrl gui=underline guifg=#4c547e cterm=underline ctermfg=4
hi MarkdownLinkText gui=none guifg=#5c5d8c cterm=none ctermfg=12
hi MarkdownLinkDelimiter gui=bold guifg=#525470 cterm=bold ctermfg=0
hi default link MarkdownLinkTextDelimiter MarkdownLinkDelimiter

hi MarkdownCode guifg=#883c64 ctermfg=5
hi default link MarkdownCodeDelimiter MarkdownCode

hi MarkdownCodeBlock guifg=#525470 ctermfg=0

hi MarkdownListMarker gui=none guifg=#306130 cterm=none ctermfg=2
hi default link MarkdownOrderedListMarker MarkdownListMarker
