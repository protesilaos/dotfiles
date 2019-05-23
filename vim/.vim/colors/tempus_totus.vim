" Name: Tempus Totus
" Description: Light theme for prose or for coding in an open space (WCAG AAA compliant)
" Author: Protesilaos Stavrou (https://protesilaos.com)
" Meta: Created with the Tempus Themes Generator
" URL: https://gitlab.com/protesilaos/tempus-themes-generator

set background=light
hi clear
if exists("syntax_on")
	syntax reset
endif
let g:colors_name = "tempus_totus"

" General
" -------
" NOTE the ctermbg=none is for terminals with transparency
hi Normal guibg=#ffffff guifg=#4a484d ctermbg=none ctermfg=0
hi Visual guibg=#4a484d guifg=#ffffff ctermbg=0 ctermfg=15
hi Search gui=underline,bold,reverse cterm=underline,bold,reverse
hi IncSearch gui=underline,bold,reverse cterm=underline,bold,reverse

hi StatusLine gui=none guibg=#4a484d guifg=#ffffff cterm=none ctermbg=0 ctermfg=15
hi StatusLineNC gui=none guibg=#f3f1f3 guifg=#5f4d4f cterm=none ctermbg=7 ctermfg=8
hi StatusLineTerm gui=none guibg=#0d5f0f guifg=#ffffff cterm=none ctermbg=2 ctermfg=15
hi StatusLineTermNC gui=none guibg=#f3f1f3 guifg=#0d5f0f cterm=none ctermbg=7 ctermfg=2

hi VertSplit gui=none cterm=none
hi TabLine gui=none guibg=#f3f1f3 guifg=#5f4d4f cterm=none ctermbg=7 ctermfg=8
hi TabLineSel gui=none guibg=#185870 guifg=#ffffff cterm=none ctermbg=6 ctermfg=15
hi TabLineFill gui=none cterm=none

hi Comment gui=italic guifg=#5f4d4f cterm=none ctermfg=8
hi Todo gui=bold guibg=#f3f1f3 guifg=#8b3800 cterm=bold ctermbg=7 ctermfg=11

hi Warning gui=none guibg=#774600 guifg=#ffffff cterm=none ctermbg=3 ctermfg=15
hi WarningMsg gui=none guibg=#774600 guifg=#ffffff cterm=none ctermbg=3 ctermfg=15
hi Error gui=none guibg=#a01c10 guifg=#ffffff cterm=none ctermbg=1 ctermfg=15
hi ErrorMsg gui=none guibg=#a01c10 guifg=#ffffff cterm=none ctermbg=1 ctermfg=15

hi MatchParen gui=underline,bold guibg=#f3f1f3 guifg=#5f4d4f cterm=underline,bold ctermbg=7 ctermfg=8

hi ToolbarLine guibg=#5f4d4f guifg=#ffffff term=none ctermbg=8 ctermfg=15
hi ToolbarButton gui=bold guibg=#5f4d4f guifg=#ffffff term=none cterm=bold ctermbg=8 ctermfg=15

hi WildMenu guibg=#ffffff guifg=#4a484d term=standout ctermbg=15 ctermfg=0

hi Terminal guibg=#ffffff guifg=#4a484d term=none ctermbg=15 ctermfg=0

" Constructs
" ----------
hi Constant guifg=#1c4f9f ctermfg=4
hi Number guifg=#1c4f9f ctermfg=4
hi Float guifg=#1c4f9f ctermfg=4
hi String guifg=#5440a7 ctermfg=12

hi Function guifg=#912663 ctermfg=5
hi Identifier guifg=#9d105b term=none ctermfg=13
hi Label guifg=#912663 ctermfg=5
hi Tag guifg=#912663 ctermfg=5
hi Keyword gui=bold guifg=#9d105b gui=bold ctermfg=13

hi Character gui=bold guifg=#005e3b cterm=bold ctermfg=14

hi Type gui=none,bold guifg=#185870 term=none cterm=none,bold ctermfg=6
hi Boolean guifg=#185870 ctermfg=6
hi StorageClass guifg=#185870 ctermfg=6
hi Structure guifg=#185870 ctermfg=6
hi Typedef gui=bold guifg=#005e3b cterm=bold ctermfg=14

hi Conditional gui=bold guifg=#0d5f0f cterm=bold ctermfg=2
hi Statement gui=none guifg=#2a5d08 cterm=none ctermfg=10
hi Repeat gui=bold guifg=#2a5d08 cterm=bold ctermfg=10
hi Operator gui=bold guifg=#4a484d cterm=bold ctermfg=0
hi Exception gui=bold guifg=#a01c10 cterm=bold ctermfg=1

hi Preproc gui=none guifg=#9a2140 term=none cterm=none ctermfg=9
hi PreCondit gui=bold guifg=#9a2140 cterm=bold ctermfg=9
hi Macro gui=bold guifg=#9a2140 cterm=bold ctermfg=9
hi Include guifg=#9a2140 ctermfg=9
hi Define guifg=#9a2140 ctermfg=9

hi Title gui=bold guibg=#ffffff guifg=#185870 cterm=bold ctermbg=15 ctermfg=6

hi Delimeter gui=bold guifg=#912663 cterm=bold ctermfg=5
hi Delimiter gui=bold guifg=#912663 cterm=bold ctermfg=5
hi SpecialComment gui=bold guifg=#912663 cterm=bold ctermfg=5

hi Debug guifg=#9d105b ctermfg=13

" Other
" -----
hi LineNr guibg=#f3f1f3 guifg=#5f4d4f term=none ctermbg=7 ctermfg=8
hi Cursor guibg=#4a484d guifg=#ffffff ctermbg=0 ctermfg=15
hi CursorLine gui=none guibg=NONE term=none cterm=none ctermbg=none
hi CursorColumn gui=none guibg=#f3f1f3 term=none cterm=none ctermbg=7
hi CursorLineNr gui=bold guibg=#5f4d4f guifg=#ffffff cterm=bold ctermbg=8 ctermfg=15
hi ColorColumn guibg=#f3f1f3 guifg=#4a484d term=none ctermbg=7 ctermfg=0

hi Folded guibg=#f3f1f3 guifg=#5f4d4f ctermbg=7 ctermfg=8
hi FoldColumn guibg=#f3f1f3 guifg=#5f4d4f ctermbg=7 ctermfg=8

hi Special gui=bold guifg=#8b3800 term=none cterm=bold ctermfg=11
hi SpecialKey gui=none guibg=#f3f1f3 guifg=#5f4d4f cterm=none ctermbg=7 ctermfg=8
hi SpecialChar gui=bold guifg=#8b3800 cterm=bold ctermfg=11
hi NonText gui=none guibg=#f3f1f3 guifg=#5f4d4f cterm=none ctermbg=7 ctermfg=8
hi EndOfBuffer gui=bold guifg=#5f4d4f cterm=bold ctermfg=8

hi Directory gui=none guifg=#0d5f0f term=none cterm=none ctermfg=2
hi Question gui=bold guifg=#8b3800 cterm=bold ctermfg=11
hi MoreMsg guifg=#2a5d08 ctermfg=10
hi ModeMsg gui=bold guifg=#0d5f0f cterm=bold ctermfg=2

hi VimOption guifg=#912663 ctermfg=5
hi VimGroup guifg=#912663 ctermfg=5

hi Underlined gui=underline guifg=#4a484d cterm=underline ctermfg=0
hi Ignore guibg=#f3f1f3 guifg=#5f4d4f ctermbg=7 ctermfg=8
hi Conceal guibg=#5f4d4f guifg=#f3f1f3 ctermbg=8 ctermfg=7

hi SpellBad guibg=#a01c10 guifg=#ffffff ctermbg=1 ctermfg=15
hi SpellCap guibg=#774600 guifg=#ffffff ctermbg=3 ctermfg=15
hi SpellRare guibg=#9d105b guifg=#ffffff ctermbg=13 ctermfg=15
hi SpellLocal guibg=#005e3b guifg=#ffffff ctermbg=14 ctermfg=15

hi Pmenu gui=italic guibg=#f3f1f3 guifg=#4a484d cterm=none ctermbg=7 ctermfg=0
hi PmenuSel gui=none,bold guibg=#5f4d4f guifg=#ffffff cterm=none,bold ctermbg=8 ctermfg=15
hi PmenuSbar guibg=#f3f1f3 ctermbg=7
hi PmenuThumb guibg=#5f4d4f ctermbg=8

" Diffs
" -----
hi DiffAdd gui=bold guibg=#0d5f0f guifg=#ffffff cterm=bold ctermbg=10 ctermfg=15
hi DiffDelete gui=none guibg=#a01c10 guifg=#ffffff cterm=none ctermbg=9 ctermfg=15
hi DiffChange gui=bold guibg=#f3f1f3 guifg=#5f4d4f cterm=bold ctermbg=7 ctermfg=8
hi DiffText gui=bold guibg=#f3f1f3 guifg=#9a2140 cterm=bold ctermbg=7 ctermfg=1

hi diffAdded guifg=#0d5f0f ctermfg=2
hi diffRemoved guifg=#a01c10 ctermfg=1
hi diffNewFile gui=none guifg=#1c4f9f ctermfg=4
hi diffFile gui=none guifg=#774600 cterm=none ctermfg=3

" Markdown
" --------
hi MarkdownRule gui=bold guibg=#f3f1f3 guifg=#2a5d08 cterm=bold ctermbg=7 ctermfg=10

hi MarkdownHeading gui=bold guifg=#4a484d cterm=bold ctermfg=0
hi default link MarkdownH1 MarkdownHeading
hi default link MarkdownH2 MarkdownHeading
hi default link MarkdownH3 MarkdownHeading
hi default link MarkdownH4 MarkdownHeading
hi default link MarkdownH5 MarkdownHeading
hi default link MarkdownH6 MarkdownHeading
hi default link MarkdownHeadingDelimiter MarkdownHeading
hi default link MarkdownHeadingRule MarkdownHeading

hi MarkdownBold gui=bold guifg=#9a2140 cterm=bold ctermfg=9
hi default link MarkdownBoldDelimiter MarkdownBold

hi MarkdownItalic gui=italic guifg=#774600 cterm=none ctermfg=3
hi default link MarkdownItalicDelimiter MarkdownItalic

hi MarkdownUrl gui=underline guifg=#1c4f9f cterm=underline ctermfg=4
hi MarkdownLinkText gui=none guifg=#5440a7 cterm=none ctermfg=12
hi MarkdownLinkDelimiter gui=bold guifg=#4a484d cterm=bold ctermfg=0
hi default link MarkdownLinkTextDelimiter MarkdownLinkDelimiter

hi MarkdownCode guifg=#912663 ctermfg=5
hi default link MarkdownCodeDelimiter MarkdownCode

hi MarkdownCodeBlock guifg=#4a484d ctermfg=0

hi MarkdownListMarker gui=none guifg=#0d5f0f cterm=none ctermfg=2
hi default link MarkdownOrderedListMarker MarkdownListMarker
