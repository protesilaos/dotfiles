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
if exists("g:tempus_enforce_background_color")
	hi Normal guibg=#f8f2e5 guifg=#464340 ctermbg=15 ctermfg=0
else
	" NOTE the ctermbg=none is for terminals with transparency
	hi Normal guibg=#f8f2e5 guifg=#464340 ctermbg=none ctermfg=0
endif

hi Visual guibg=#464340 guifg=#f8f2e5 ctermbg=0 ctermfg=15
hi Search gui=underline,bold guibg=#e7e3d7 guifg=#464340 cterm=underline,bold ctermbg=7 ctermfg=3
hi IncSearch gui=underline,bold guibg=#68607d guifg=#f8f2e5 term=none cterm=underline,bold ctermbg=8 ctermfg=15

hi StatusLine gui=none guibg=#464340 guifg=#f8f2e5 cterm=none ctermbg=0 ctermfg=15
hi StatusLineNC gui=none guibg=#e7e3d7 guifg=#68607d cterm=none ctermbg=7 ctermfg=8
hi StatusLineTerm gui=none guibg=#107410 guifg=#f8f2e5 cterm=none ctermbg=2 ctermfg=15
hi StatusLineTermNC gui=none guibg=#e7e3d7 guifg=#107410 cterm=none ctermbg=7 ctermfg=2

hi VertSplit gui=none cterm=none
hi TabLine gui=none guibg=#e7e3d7 guifg=#68607d cterm=none ctermbg=7 ctermfg=8
hi TabLineSel gui=none guibg=#007070 guifg=#f8f2e5 cterm=none ctermbg=6 ctermfg=15
hi TabLineFill gui=none cterm=none

hi Comment gui=italic guifg=#68607d cterm=none ctermfg=8
hi Todo gui=bold guibg=#e7e3d7 guifg=#6f6600 cterm=bold ctermbg=7 ctermfg=11

hi Warning gui=none guibg=#806000 guifg=#f8f2e5 cterm=none ctermbg=3 ctermfg=15
hi WarningMsg gui=none guibg=#806000 guifg=#f8f2e5 cterm=none ctermbg=3 ctermfg=15
hi Error gui=none guibg=#c81000 guifg=#f8f2e5 cterm=none ctermbg=1 ctermfg=15
hi ErrorMsg gui=none guibg=#c81000 guifg=#f8f2e5 cterm=none ctermbg=1 ctermfg=15

hi MatchParen gui=underline,bold guibg=#e7e3d7 guifg=#68607d cterm=underline,bold ctermbg=7 ctermfg=8

hi ToolbarLine guibg=#68607d guifg=#f8f2e5 term=none ctermbg=8 ctermfg=15
hi ToolbarButton gui=bold guibg=#68607d guifg=#f8f2e5 term=none cterm=bold ctermbg=8 ctermfg=15

hi WildMenu guibg=#f8f2e5 guifg=#464340 term=standout ctermbg=15 ctermfg=0

hi Terminal guibg=#f8f2e5 guifg=#464340 term=none ctermbg=15 ctermfg=0

" Constructs
" ----------
hi Constant guifg=#385dc4 ctermfg=4
hi Number guifg=#385dc4 ctermfg=4
hi Float guifg=#385dc4 ctermfg=4
hi String guifg=#0f64c4 ctermfg=12

hi Function guifg=#b63052 ctermfg=5
hi Identifier guifg=#8050a7 term=none ctermfg=13
hi Label guifg=#b63052 ctermfg=5
hi Tag guifg=#b63052 ctermfg=5
hi Keyword gui=bold guifg=#8050a7 gui=bold ctermfg=13

hi Character gui=bold guifg=#336c87 cterm=bold ctermfg=14

hi Type gui=none,bold guifg=#007070 term=none cterm=none,bold ctermfg=6
hi Boolean guifg=#007070 ctermfg=6
hi StorageClass guifg=#007070 ctermfg=6
hi Structure guifg=#007070 ctermfg=6
hi Typedef gui=bold guifg=#336c87 cterm=bold ctermfg=14

hi Conditional gui=bold guifg=#107410 cterm=bold ctermfg=2
hi Statement gui=none guifg=#427040 cterm=none ctermfg=10
hi Repeat gui=bold guifg=#427040 cterm=bold ctermfg=10
hi Operator gui=bold guifg=#464340 cterm=bold ctermfg=0
hi Exception gui=bold guifg=#c81000 cterm=bold ctermfg=1

hi Preproc gui=none guifg=#b24000 term=none cterm=none ctermfg=9
hi PreCondit gui=bold guifg=#b24000 cterm=bold ctermfg=9
hi Macro gui=bold guifg=#b24000 cterm=bold ctermfg=9
hi Include guifg=#b24000 ctermfg=9
hi Define guifg=#b24000 ctermfg=9

hi Title gui=bold guibg=#f8f2e5 guifg=#007070 cterm=bold ctermbg=15 ctermfg=6

hi Delimeter gui=bold guifg=#b63052 cterm=bold ctermfg=5
hi Delimiter gui=bold guifg=#b63052 cterm=bold ctermfg=5
hi SpecialComment gui=bold guifg=#b63052 cterm=bold ctermfg=5

hi Debug guifg=#8050a7 ctermfg=13

" Other
" -----
hi LineNr guibg=#e7e3d7 guifg=#68607d term=none ctermbg=7 ctermfg=8
hi Cursor guibg=#464340 guifg=#f8f2e5 ctermbg=0 ctermfg=15
hi CursorLine gui=none guibg=#e7e3d7 term=none cterm=none ctermbg=7
hi CursorColumn gui=none guibg=#e7e3d7 term=none cterm=none ctermbg=7
hi CursorLineNr gui=bold guibg=#68607d guifg=#f8f2e5 cterm=bold ctermbg=8 ctermfg=15
hi ColorColumn guibg=#e7e3d7 guifg=#464340 term=none ctermbg=7 ctermfg=0
hi SignColumn guibg=#e7e3d7 guifg=#68607d term=none ctermbg=7 ctermfg=8

hi Folded guibg=#e7e3d7 guifg=#68607d ctermbg=7 ctermfg=8
hi FoldColumn guibg=#e7e3d7 guifg=#68607d ctermbg=7 ctermfg=8

hi Special gui=bold guifg=#6f6600 term=none cterm=bold ctermfg=11
hi SpecialKey gui=none guibg=#e7e3d7 guifg=#68607d cterm=none ctermbg=7 ctermfg=8
hi SpecialChar gui=bold guifg=#6f6600 cterm=bold ctermfg=11
hi NonText gui=none guibg=#e7e3d7 guifg=#68607d cterm=none ctermbg=7 ctermfg=8
hi EndOfBuffer gui=bold guifg=#68607d cterm=bold ctermfg=8

hi Directory gui=none guifg=#107410 term=none cterm=none ctermfg=2
hi Question gui=bold guifg=#6f6600 cterm=bold ctermfg=11
hi MoreMsg guifg=#427040 ctermfg=10
hi ModeMsg gui=bold guifg=#107410 cterm=bold ctermfg=2

hi VimOption guifg=#b63052 ctermfg=5
hi VimGroup guifg=#b63052 ctermfg=5

hi Underlined gui=underline guifg=#464340 cterm=underline ctermfg=0
hi Ignore guibg=#e7e3d7 guifg=#68607d ctermbg=7 ctermfg=8
hi Conceal guibg=#68607d guifg=#e7e3d7 ctermbg=8 ctermfg=7

hi SpellBad guibg=#c81000 guifg=#f8f2e5 ctermbg=1 ctermfg=15
hi SpellCap guibg=#806000 guifg=#f8f2e5 ctermbg=3 ctermfg=15
hi SpellRare guibg=#8050a7 guifg=#f8f2e5 ctermbg=13 ctermfg=15
hi SpellLocal guibg=#336c87 guifg=#f8f2e5 ctermbg=14 ctermfg=15

hi Pmenu gui=italic guibg=#e7e3d7 guifg=#464340 cterm=none ctermbg=7 ctermfg=0
hi PmenuSel gui=none,bold guibg=#68607d guifg=#f8f2e5 cterm=none,bold ctermbg=8 ctermfg=15
hi PmenuSbar guibg=#e7e3d7 ctermbg=7
hi PmenuThumb guibg=#68607d ctermbg=8

" Terminal
" --------

if exists('*term_setansicolors')
	let g:terminal_ansi_colors = [
				\ "#464340",
				\ "#c81000",
				\ "#107410",
				\ "#806000",
				\ "#385dc4",
				\ "#b63052",
				\ "#007070",
				\ "#e7e3d7",
				\ "#68607d",
				\ "#b24000",
				\ "#427040",
				\ "#6f6600",
				\ "#0f64c4",
				\ "#8050a7",
				\ "#336c87",
				\ "#f8f2e5"
				\ ]
endif
if has('nvim')
	let g:terminal_color_0 = "#464340"
	let g:terminal_color_1 = "#c81000"
	let g:terminal_color_2 = "#107410"
	let g:terminal_color_3 = "#806000"
	let g:terminal_color_4 = "#385dc4"
	let g:terminal_color_5 = "#b63052"
	let g:terminal_color_6 = "#007070"
	let g:terminal_color_7 = "#e7e3d7"
	let g:terminal_color_8 = "#68607d"
	let g:terminal_color_9 = "#b24000"
	let g:terminal_color_10 = "#427040"
	let g:terminal_color_11 = "#6f6600"
	let g:terminal_color_12 = "#0f64c4"
	let g:terminal_color_13 = "#8050a7"
	let g:terminal_color_14 = "#336c87"
	let g:terminal_color_15 = "#f8f2e5"
endif

" Diffs
" -----
hi DiffAdd gui=bold guibg=#107410 guifg=#f8f2e5 cterm=bold ctermbg=10 ctermfg=15
hi DiffDelete gui=none guibg=#c81000 guifg=#f8f2e5 cterm=none ctermbg=9 ctermfg=15
hi DiffChange gui=bold guibg=#e7e3d7 guifg=#68607d cterm=bold ctermbg=7 ctermfg=8
hi DiffText gui=bold guibg=#e7e3d7 guifg=#b24000 cterm=bold ctermbg=7 ctermfg=1

hi diffAdded guifg=#107410 ctermfg=2
hi diffRemoved guifg=#c81000 ctermfg=1
hi diffNewFile gui=none guifg=#385dc4 ctermfg=4
hi diffFile gui=none guifg=#806000 cterm=none ctermfg=3

hi GitGutterAdd guibg=#e7e3d7 guifg=#107410 ctermbg=7 ctermfg=2
hi GitGutterChange gui=bold guibg=#e7e3d7 guifg=#68607d cterm=bold ctermbg=7 ctermfg=8
hi GitGutterDelete guibg=#e7e3d7 guifg=#c81000 ctermbg=7 ctermfg=1
hi GitGutterChangeDelete gui=bold guibg=#e7e3d7 guifg=#c81000 cterm=bold ctermbg=7 ctermfg=1

" Neomake
" -------
hi NeomakeError gui=none guibg=#c81000 guifg=#f8f2e5 cterm=none ctermbg=1 ctermfg=15
hi NeomakeInfo gui=none guibg=#007070 guifg=#f8f2e5 cterm=none ctermbg=6 ctermfg=15
hi NeomakeWarning gui=none guibg=#806000 guifg=#f8f2e5 cterm=none ctermbg=3 ctermfg=15
hi NeomakeMessage gui=none guibg=#6f6600 guifg=#f8f2e5 cterm=none ctermbg=11 ctermfg=15

hi NeomakeVirtualtextInfoDefault guifg=#336c87 ctermfg=14
hi NeomakeVirtualtextMessageDefault guifg=#6f6600 ctermfg=11
hi NeomakeVirtualtextWarningDefault guifg=#806000 ctermfg=3
hi NeomakeVirtualtextErrorDefault guifg=#c81000 ctermfg=1

hi NeomakeStatusGood gui=none guibg=#107410 guifg=#f8f2e5 cterm=none ctermbg=2 ctermfg=15
hi NeomakeStatusGoodNC gui=none guibg=#e7e3d7 guifg=#107410 cterm=none ctermbg=7 ctermfg=2

hi NeomakeStatColorDefault gui=none guibg=#385dc4 guifg=#f8f2e5 cterm=none ctermbg=4 ctermfg=15
hi NeomakeStatColorTypeE gui=none guibg=#c81000 guifg=#f8f2e5 cterm=none ctermbg=1 ctermfg=15
hi NeomakeStatColorTypeW gui=none guibg=#806000 guifg=#f8f2e5 cterm=none ctermbg=3 ctermfg=15

" Markdown
" --------
hi MarkdownRule gui=bold guibg=#e7e3d7 guifg=#427040 cterm=bold ctermbg=7 ctermfg=10

hi MarkdownHeading gui=bold guifg=#464340 cterm=bold ctermfg=0
hi default link MarkdownH1 MarkdownHeading
hi default link MarkdownH2 MarkdownHeading
hi default link MarkdownH3 MarkdownHeading
hi default link MarkdownH4 MarkdownHeading
hi default link MarkdownH5 MarkdownHeading
hi default link MarkdownH6 MarkdownHeading
hi default link MarkdownHeadingDelimiter MarkdownHeading
hi default link MarkdownHeadingRule MarkdownHeading

hi MarkdownBold gui=bold guifg=#b24000 cterm=bold ctermfg=9
hi default link MarkdownBoldDelimiter MarkdownBold

hi MarkdownItalic gui=italic guifg=#806000 cterm=none ctermfg=3
hi default link MarkdownItalicDelimiter MarkdownItalic

hi MarkdownUrl gui=underline guifg=#385dc4 cterm=underline ctermfg=4
hi MarkdownLinkText gui=none guifg=#0f64c4 cterm=none ctermfg=12
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
