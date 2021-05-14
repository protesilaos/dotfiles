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
if exists("g:tempus_enforce_background_color")
	hi Normal guibg=#ffffff guifg=#4a484d ctermbg=15 ctermfg=0
else
	" NOTE the ctermbg=none is for terminals with transparency
	hi Normal guibg=#ffffff guifg=#4a484d ctermbg=none ctermfg=0
endif

hi Visual guibg=#4a484d guifg=#ffffff ctermbg=0 ctermfg=15
hi Search gui=underline,bold guibg=#efefef guifg=#4a484d cterm=underline,bold ctermbg=7 ctermfg=3
hi IncSearch gui=underline,bold guibg=#5e4b4f guifg=#ffffff term=none cterm=underline,bold ctermbg=8 ctermfg=15

hi StatusLine gui=none guibg=#4a484d guifg=#ffffff cterm=none ctermbg=0 ctermfg=15
hi StatusLineNC gui=none guibg=#efefef guifg=#5e4b4f cterm=none ctermbg=7 ctermfg=8
hi StatusLineTerm gui=none guibg=#005d26 guifg=#ffffff cterm=none ctermbg=2 ctermfg=15
hi StatusLineTermNC gui=none guibg=#efefef guifg=#005d26 cterm=none ctermbg=7 ctermfg=2

hi VertSplit gui=none cterm=none
hi TabLine gui=none guibg=#efefef guifg=#5e4b4f cterm=none ctermbg=7 ctermfg=8
hi TabLineSel gui=none guibg=#185570 guifg=#ffffff cterm=none ctermbg=6 ctermfg=15
hi TabLineFill gui=none cterm=none

hi Comment gui=italic guifg=#5e4b4f cterm=none ctermfg=8
hi Todo gui=bold guibg=#efefef guifg=#8a3600 cterm=bold ctermbg=7 ctermfg=11

hi Warning gui=none guibg=#714700 guifg=#ffffff cterm=none ctermbg=3 ctermfg=15
hi WarningMsg gui=none guibg=#714700 guifg=#ffffff cterm=none ctermbg=3 ctermfg=15
hi Error gui=none guibg=#a50000 guifg=#ffffff cterm=none ctermbg=1 ctermfg=15
hi ErrorMsg gui=none guibg=#a50000 guifg=#ffffff cterm=none ctermbg=1 ctermfg=15

hi MatchParen gui=underline,bold guibg=#efefef guifg=#5e4b4f cterm=underline,bold ctermbg=7 ctermfg=8

hi ToolbarLine guibg=#5e4b4f guifg=#ffffff term=none ctermbg=8 ctermfg=15
hi ToolbarButton gui=bold guibg=#5e4b4f guifg=#ffffff term=none cterm=bold ctermbg=8 ctermfg=15

hi WildMenu guibg=#ffffff guifg=#4a484d term=standout ctermbg=15 ctermfg=0

hi Terminal guibg=#ffffff guifg=#4a484d term=none ctermbg=15 ctermfg=0

" Constructs
" ----------
hi Constant guifg=#1d3ccf ctermfg=4
hi Number guifg=#1d3ccf ctermfg=4
hi Float guifg=#1d3ccf ctermfg=4
hi String guifg=#2d45b0 ctermfg=12

hi Function guifg=#88267a ctermfg=5
hi Identifier guifg=#700dc9 term=none ctermfg=13
hi Label guifg=#88267a ctermfg=5
hi Tag guifg=#88267a ctermfg=5
hi Keyword gui=bold guifg=#700dc9 gui=bold ctermfg=13

hi Character gui=bold guifg=#005289 cterm=bold ctermfg=14

hi Type gui=none,bold guifg=#185570 term=none cterm=none,bold ctermfg=6
hi Boolean guifg=#185570 ctermfg=6
hi StorageClass guifg=#185570 ctermfg=6
hi Structure guifg=#185570 ctermfg=6
hi Typedef gui=bold guifg=#005289 cterm=bold ctermfg=14

hi Conditional gui=bold guifg=#005d26 cterm=bold ctermfg=2
hi Statement gui=none guifg=#4a5500 cterm=none ctermfg=10
hi Repeat gui=bold guifg=#4a5500 cterm=bold ctermfg=10
hi Operator gui=bold guifg=#4a484d cterm=bold ctermfg=0
hi Exception gui=bold guifg=#a50000 cterm=bold ctermfg=1

hi Preproc gui=none guifg=#992030 term=none cterm=none ctermfg=9
hi PreCondit gui=bold guifg=#992030 cterm=bold ctermfg=9
hi Macro gui=bold guifg=#992030 cterm=bold ctermfg=9
hi Include guifg=#992030 ctermfg=9
hi Define guifg=#992030 ctermfg=9

hi Title gui=bold guibg=#ffffff guifg=#185570 cterm=bold ctermbg=15 ctermfg=6

hi Delimeter gui=bold guifg=#88267a cterm=bold ctermfg=5
hi Delimiter gui=bold guifg=#88267a cterm=bold ctermfg=5
hi SpecialComment gui=bold guifg=#88267a cterm=bold ctermfg=5

hi Debug guifg=#700dc9 ctermfg=13

" Other
" -----
hi LineNr guibg=#efefef guifg=#5e4b4f term=none ctermbg=7 ctermfg=8
hi Cursor guibg=#4a484d guifg=#ffffff ctermbg=0 ctermfg=15
hi CursorLine gui=none guibg=#efefef term=none cterm=none ctermbg=7
hi CursorColumn gui=none guibg=#efefef term=none cterm=none ctermbg=7
hi CursorLineNr gui=bold guibg=#5e4b4f guifg=#ffffff cterm=bold ctermbg=8 ctermfg=15
hi ColorColumn guibg=#efefef guifg=#4a484d term=none ctermbg=7 ctermfg=0
hi SignColumn guibg=#efefef guifg=#5e4b4f term=none ctermbg=7 ctermfg=8

hi Folded guibg=#efefef guifg=#5e4b4f ctermbg=7 ctermfg=8
hi FoldColumn guibg=#efefef guifg=#5e4b4f ctermbg=7 ctermfg=8

hi Special gui=bold guifg=#8a3600 term=none cterm=bold ctermfg=11
hi SpecialKey gui=none guibg=#efefef guifg=#5e4b4f cterm=none ctermbg=7 ctermfg=8
hi SpecialChar gui=bold guifg=#8a3600 cterm=bold ctermfg=11
hi NonText gui=none guibg=#efefef guifg=#5e4b4f cterm=none ctermbg=7 ctermfg=8
hi EndOfBuffer gui=bold guifg=#5e4b4f cterm=bold ctermfg=8

hi Directory gui=none guifg=#005d26 term=none cterm=none ctermfg=2
hi Question gui=bold guifg=#8a3600 cterm=bold ctermfg=11
hi MoreMsg guifg=#4a5500 ctermfg=10
hi ModeMsg gui=bold guifg=#005d26 cterm=bold ctermfg=2

hi VimOption guifg=#88267a ctermfg=5
hi VimGroup guifg=#88267a ctermfg=5

hi Underlined gui=underline guifg=#4a484d cterm=underline ctermfg=0
hi Ignore guibg=#efefef guifg=#5e4b4f ctermbg=7 ctermfg=8
hi Conceal guibg=#5e4b4f guifg=#efefef ctermbg=8 ctermfg=7

hi SpellBad guibg=#a50000 guifg=#ffffff ctermbg=1 ctermfg=15
hi SpellCap guibg=#714700 guifg=#ffffff ctermbg=3 ctermfg=15
hi SpellRare guibg=#700dc9 guifg=#ffffff ctermbg=13 ctermfg=15
hi SpellLocal guibg=#005289 guifg=#ffffff ctermbg=14 ctermfg=15

hi Pmenu gui=italic guibg=#efefef guifg=#4a484d cterm=none ctermbg=7 ctermfg=0
hi PmenuSel gui=none,bold guibg=#5e4b4f guifg=#ffffff cterm=none,bold ctermbg=8 ctermfg=15
hi PmenuSbar guibg=#efefef ctermbg=7
hi PmenuThumb guibg=#5e4b4f ctermbg=8

" Terminal
" --------

if exists('*term_setansicolors')
	let g:terminal_ansi_colors = [
				\ "#4a484d",
				\ "#a50000",
				\ "#005d26",
				\ "#714700",
				\ "#1d3ccf",
				\ "#88267a",
				\ "#185570",
				\ "#efefef",
				\ "#5e4b4f",
				\ "#992030",
				\ "#4a5500",
				\ "#8a3600",
				\ "#2d45b0",
				\ "#700dc9",
				\ "#005289",
				\ "#ffffff"
				\ ]
endif
if has('nvim')
	let g:terminal_color_0 = "#4a484d"
	let g:terminal_color_1 = "#a50000"
	let g:terminal_color_2 = "#005d26"
	let g:terminal_color_3 = "#714700"
	let g:terminal_color_4 = "#1d3ccf"
	let g:terminal_color_5 = "#88267a"
	let g:terminal_color_6 = "#185570"
	let g:terminal_color_7 = "#efefef"
	let g:terminal_color_8 = "#5e4b4f"
	let g:terminal_color_9 = "#992030"
	let g:terminal_color_10 = "#4a5500"
	let g:terminal_color_11 = "#8a3600"
	let g:terminal_color_12 = "#2d45b0"
	let g:terminal_color_13 = "#700dc9"
	let g:terminal_color_14 = "#005289"
	let g:terminal_color_15 = "#ffffff"
endif

" Diffs
" -----
hi DiffAdd gui=bold guibg=#005d26 guifg=#ffffff cterm=bold ctermbg=10 ctermfg=15
hi DiffDelete gui=none guibg=#a50000 guifg=#ffffff cterm=none ctermbg=9 ctermfg=15
hi DiffChange gui=bold guibg=#efefef guifg=#5e4b4f cterm=bold ctermbg=7 ctermfg=8
hi DiffText gui=bold guibg=#efefef guifg=#992030 cterm=bold ctermbg=7 ctermfg=1

hi diffAdded guifg=#005d26 ctermfg=2
hi diffRemoved guifg=#a50000 ctermfg=1
hi diffNewFile gui=none guifg=#1d3ccf ctermfg=4
hi diffFile gui=none guifg=#714700 cterm=none ctermfg=3

hi GitGutterAdd guibg=#efefef guifg=#005d26 ctermbg=7 ctermfg=2
hi GitGutterChange gui=bold guibg=#efefef guifg=#5e4b4f cterm=bold ctermbg=7 ctermfg=8
hi GitGutterDelete guibg=#efefef guifg=#a50000 ctermbg=7 ctermfg=1
hi GitGutterChangeDelete gui=bold guibg=#efefef guifg=#a50000 cterm=bold ctermbg=7 ctermfg=1

" Neomake
" -------
hi NeomakeError gui=none guibg=#a50000 guifg=#ffffff cterm=none ctermbg=1 ctermfg=15
hi NeomakeInfo gui=none guibg=#185570 guifg=#ffffff cterm=none ctermbg=6 ctermfg=15
hi NeomakeWarning gui=none guibg=#714700 guifg=#ffffff cterm=none ctermbg=3 ctermfg=15
hi NeomakeMessage gui=none guibg=#8a3600 guifg=#ffffff cterm=none ctermbg=11 ctermfg=15

hi NeomakeVirtualtextInfoDefault guifg=#005289 ctermfg=14
hi NeomakeVirtualtextMessageDefault guifg=#8a3600 ctermfg=11
hi NeomakeVirtualtextWarningDefault guifg=#714700 ctermfg=3
hi NeomakeVirtualtextErrorDefault guifg=#a50000 ctermfg=1

hi NeomakeStatusGood gui=none guibg=#005d26 guifg=#ffffff cterm=none ctermbg=2 ctermfg=15
hi NeomakeStatusGoodNC gui=none guibg=#efefef guifg=#005d26 cterm=none ctermbg=7 ctermfg=2

hi NeomakeStatColorDefault gui=none guibg=#1d3ccf guifg=#ffffff cterm=none ctermbg=4 ctermfg=15
hi NeomakeStatColorTypeE gui=none guibg=#a50000 guifg=#ffffff cterm=none ctermbg=1 ctermfg=15
hi NeomakeStatColorTypeW gui=none guibg=#714700 guifg=#ffffff cterm=none ctermbg=3 ctermfg=15

" Markdown
" --------
hi MarkdownRule gui=bold guibg=#efefef guifg=#4a5500 cterm=bold ctermbg=7 ctermfg=10

hi MarkdownHeading gui=bold guifg=#4a484d cterm=bold ctermfg=0
hi default link MarkdownH1 MarkdownHeading
hi default link MarkdownH2 MarkdownHeading
hi default link MarkdownH3 MarkdownHeading
hi default link MarkdownH4 MarkdownHeading
hi default link MarkdownH5 MarkdownHeading
hi default link MarkdownH6 MarkdownHeading
hi default link MarkdownHeadingDelimiter MarkdownHeading
hi default link MarkdownHeadingRule MarkdownHeading

hi MarkdownBold gui=bold guifg=#992030 cterm=bold ctermfg=9
hi default link MarkdownBoldDelimiter MarkdownBold

hi MarkdownItalic gui=italic guifg=#714700 cterm=none ctermfg=3
hi default link MarkdownItalicDelimiter MarkdownItalic

hi MarkdownUrl gui=underline guifg=#1d3ccf cterm=underline ctermfg=4
hi MarkdownLinkText gui=none guifg=#2d45b0 cterm=none ctermfg=12
hi MarkdownLinkDelimiter gui=bold guifg=#4a484d cterm=bold ctermfg=0
hi default link MarkdownLinkTextDelimiter MarkdownLinkDelimiter

hi MarkdownCode guifg=#88267a ctermfg=5
hi default link MarkdownCodeDelimiter MarkdownCode

hi MarkdownCodeBlock guifg=#4a484d ctermfg=0

hi MarkdownListMarker gui=none guifg=#005d26 cterm=none ctermfg=2
hi default link MarkdownOrderedListMarker MarkdownListMarker

" Linting
" -------
hi YcmErrorSection gui=undercurl guisp=#a50000 cterm=underline
hi YcmWarningSection gui=undercurl guisp=#1d3ccf cterm=underline
hi SyntasticError gui=undercurl guisp=#a50000 cterm=underline
hi SyntasticWarning gui=undercurl guisp=#1d3ccf cterm=underline
hi SyntasticErrorSing guifg=#ffffff guibg=#a50000 ctermfg=15 ctermbg=1
hi SyntasticWarningSign guifg=#ffffff guibg=#1d3ccf ctermfg=15 ctermbg=4
