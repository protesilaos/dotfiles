" Name: Tempus Rift
" Description: Dark theme with a subdued palette on the green side of the spectrum (WCAG AA compliant)
" Author: Protesilaos Stavrou (https://protesilaos.com)
" Meta: Created with the Tempus Themes Generator
" URL: https://gitlab.com/protesilaos/tempus-themes-generator

set background=dark
hi clear
if exists("syntax_on")
	syntax reset
endif
let g:colors_name = "tempus_rift"

" General
" -------
if exists("g:tempus_enforce_background_color")
	hi Normal guibg=#162c22 guifg=#bbbcbc ctermbg=0 ctermfg=15
else
	" NOTE the ctermbg=none is for terminals with transparency
	hi Normal guibg=#162c22 guifg=#bbbcbc ctermbg=none ctermfg=15
endif

hi Visual guibg=#bbbcbc guifg=#162c22 ctermbg=15 ctermfg=0
hi Search gui=underline,bold guibg=#283431 guifg=#bbbcbc cterm=underline,bold ctermbg=8 ctermfg=3
hi IncSearch gui=underline,bold guibg=#ab9aa9 guifg=#162c22 term=none cterm=underline,bold ctermbg=7 ctermfg=0

hi StatusLine gui=none guibg=#bbbcbc guifg=#162c22 cterm=none ctermbg=15 ctermfg=0
hi StatusLineNC gui=none guibg=#283431 guifg=#ab9aa9 cterm=none ctermbg=8 ctermfg=7
hi StatusLineTerm gui=none guibg=#34b534 guifg=#162c22 cterm=none ctermbg=2 ctermfg=0
hi StatusLineTermNC gui=none guibg=#283431 guifg=#34b534 cterm=none ctermbg=8 ctermfg=2

hi VertSplit gui=none cterm=none
hi TabLine gui=none guibg=#283431 guifg=#ab9aa9 cterm=none ctermbg=8 ctermfg=7
hi TabLineSel gui=none guibg=#5fad8f guifg=#162c22 cterm=none ctermbg=6 ctermfg=0
hi TabLineFill gui=none cterm=none

hi Comment gui=italic guifg=#ab9aa9 cterm=none ctermfg=7
hi Todo gui=bold guibg=#283431 guifg=#82bd00 cterm=bold ctermbg=8 ctermfg=11

hi Warning gui=none guibg=#7fad00 guifg=#162c22 cterm=none ctermbg=3 ctermfg=0
hi WarningMsg gui=none guibg=#7fad00 guifg=#162c22 cterm=none ctermbg=3 ctermfg=0
hi Error gui=none guibg=#c19904 guifg=#162c22 cterm=none ctermbg=1 ctermfg=0
hi ErrorMsg gui=none guibg=#c19904 guifg=#162c22 cterm=none ctermbg=1 ctermfg=0

hi MatchParen gui=underline,bold guibg=#283431 guifg=#ab9aa9 cterm=underline,bold ctermbg=8 ctermfg=7

hi ToolbarLine guibg=#ab9aa9 guifg=#162c22 term=none ctermbg=7 ctermfg=0
hi ToolbarButton gui=bold guibg=#ab9aa9 guifg=#162c22 term=none cterm=bold ctermbg=7 ctermfg=0

hi WildMenu guibg=#162c22 guifg=#bbbcbc term=standout ctermbg=0 ctermfg=15

hi Terminal guibg=#162c22 guifg=#bbbcbc term=none ctermbg=0 ctermfg=15

" Constructs
" ----------
hi Constant guifg=#30aeb0 ctermfg=4
hi Number guifg=#30aeb0 ctermfg=4
hi Float guifg=#30aeb0 ctermfg=4
hi String guifg=#56bdad ctermfg=12

hi Function guifg=#c8954c ctermfg=5
hi Identifier guifg=#cca0ba term=none ctermfg=13
hi Label guifg=#c8954c ctermfg=5
hi Tag guifg=#c8954c ctermfg=5
hi Keyword gui=bold guifg=#cca0ba gui=bold ctermfg=13

hi Character gui=bold guifg=#10c480 cterm=bold ctermfg=14

hi Type gui=none,bold guifg=#5fad8f term=none cterm=none,bold ctermfg=6
hi Boolean guifg=#5fad8f ctermfg=6
hi StorageClass guifg=#5fad8f ctermfg=6
hi Structure guifg=#5fad8f ctermfg=6
hi Typedef gui=bold guifg=#10c480 cterm=bold ctermfg=14

hi Conditional gui=bold guifg=#34b534 cterm=bold ctermfg=2
hi Statement gui=none guifg=#6ac134 cterm=none ctermfg=10
hi Repeat gui=bold guifg=#6ac134 cterm=bold ctermfg=10
hi Operator gui=bold guifg=#bbbcbc cterm=bold ctermfg=15
hi Exception gui=bold guifg=#c19904 cterm=bold ctermfg=1

hi Preproc gui=none guifg=#d2a634 term=none cterm=none ctermfg=9
hi PreCondit gui=bold guifg=#d2a634 cterm=bold ctermfg=9
hi Macro gui=bold guifg=#d2a634 cterm=bold ctermfg=9
hi Include guifg=#d2a634 ctermfg=9
hi Define guifg=#d2a634 ctermfg=9

hi Title gui=bold guibg=#162c22 guifg=#5fad8f cterm=bold ctermbg=0 ctermfg=6

hi Delimeter gui=bold guifg=#c8954c cterm=bold ctermfg=5
hi Delimiter gui=bold guifg=#c8954c cterm=bold ctermfg=5
hi SpecialComment gui=bold guifg=#c8954c cterm=bold ctermfg=5

hi Debug guifg=#cca0ba ctermfg=13

" Other
" -----
hi LineNr guibg=#283431 guifg=#ab9aa9 term=none ctermbg=8 ctermfg=7
hi Cursor guibg=#bbbcbc guifg=#162c22 ctermbg=15 ctermfg=0
hi CursorLine gui=none guibg=#283431 term=none cterm=none ctermbg=8
hi CursorColumn gui=none guibg=#283431 term=none cterm=none ctermbg=8
hi CursorLineNr gui=bold guibg=#ab9aa9 guifg=#162c22 cterm=bold ctermbg=7 ctermfg=0
hi ColorColumn guibg=#283431 guifg=#bbbcbc term=none ctermbg=8 ctermfg=15
hi SignColumn guibg=#283431 guifg=#ab9aa9 term=none ctermbg=8 ctermfg=7

hi Folded guibg=#283431 guifg=#ab9aa9 ctermbg=8 ctermfg=7
hi FoldColumn guibg=#283431 guifg=#ab9aa9 ctermbg=8 ctermfg=7

hi Special gui=bold guifg=#82bd00 term=none cterm=bold ctermfg=11
hi SpecialKey gui=none guibg=#283431 guifg=#ab9aa9 cterm=none ctermbg=8 ctermfg=7
hi SpecialChar gui=bold guifg=#82bd00 cterm=bold ctermfg=11
hi NonText gui=none guibg=#283431 guifg=#ab9aa9 cterm=none ctermbg=8 ctermfg=7
hi EndOfBuffer gui=bold guifg=#ab9aa9 cterm=bold ctermfg=7

hi Directory gui=none guifg=#34b534 term=none cterm=none ctermfg=2
hi Question gui=bold guifg=#82bd00 cterm=bold ctermfg=11
hi MoreMsg guifg=#6ac134 ctermfg=10
hi ModeMsg gui=bold guifg=#34b534 cterm=bold ctermfg=2

hi VimOption guifg=#c8954c ctermfg=5
hi VimGroup guifg=#c8954c ctermfg=5

hi Underlined gui=underline guifg=#bbbcbc cterm=underline ctermfg=15
hi Ignore guibg=#283431 guifg=#ab9aa9 ctermbg=8 ctermfg=7
hi Conceal guibg=#ab9aa9 guifg=#283431 ctermbg=7 ctermfg=8

hi SpellBad guibg=#c19904 guifg=#162c22 ctermbg=1 ctermfg=0
hi SpellCap guibg=#7fad00 guifg=#162c22 ctermbg=3 ctermfg=0
hi SpellRare guibg=#cca0ba guifg=#162c22 ctermbg=13 ctermfg=0
hi SpellLocal guibg=#10c480 guifg=#162c22 ctermbg=14 ctermfg=0

hi Pmenu gui=italic guibg=#283431 guifg=#bbbcbc cterm=none ctermbg=8 ctermfg=15
hi PmenuSel gui=none,bold guibg=#ab9aa9 guifg=#162c22 cterm=none,bold ctermbg=7 ctermfg=0
hi PmenuSbar guibg=#283431 ctermbg=8
hi PmenuThumb guibg=#ab9aa9 ctermbg=7

" Terminal
" --------

if exists('*term_setansicolors')
	let g:terminal_ansi_colors = [
				\ "#162c22",
				\ "#c19904",
				\ "#34b534",
				\ "#7fad00",
				\ "#30aeb0",
				\ "#c8954c",
				\ "#5fad8f",
				\ "#ab9aa9",
				\ "#283431",
				\ "#d2a634",
				\ "#6ac134",
				\ "#82bd00",
				\ "#56bdad",
				\ "#cca0ba",
				\ "#10c480",
				\ "#bbbcbc"
				\ ]
endif
if has('nvim')
	let g:terminal_color_0 = "#162c22"
	let g:terminal_color_1 = "#c19904"
	let g:terminal_color_2 = "#34b534"
	let g:terminal_color_3 = "#7fad00"
	let g:terminal_color_4 = "#30aeb0"
	let g:terminal_color_5 = "#c8954c"
	let g:terminal_color_6 = "#5fad8f"
	let g:terminal_color_7 = "#ab9aa9"
	let g:terminal_color_8 = "#283431"
	let g:terminal_color_9 = "#d2a634"
	let g:terminal_color_10 = "#6ac134"
	let g:terminal_color_11 = "#82bd00"
	let g:terminal_color_12 = "#56bdad"
	let g:terminal_color_13 = "#cca0ba"
	let g:terminal_color_14 = "#10c480"
	let g:terminal_color_15 = "#bbbcbc"
endif

" Diffs
" -----
hi DiffAdd gui=bold guibg=#34b534 guifg=#162c22 cterm=bold ctermbg=10 ctermfg=0
hi DiffDelete gui=none guibg=#c19904 guifg=#162c22 cterm=none ctermbg=9 ctermfg=0
hi DiffChange gui=bold guibg=#283431 guifg=#ab9aa9 cterm=bold ctermbg=8 ctermfg=7
hi DiffText gui=bold guibg=#283431 guifg=#d2a634 cterm=bold ctermbg=8 ctermfg=1

hi diffAdded guifg=#34b534 ctermfg=2
hi diffRemoved guifg=#c19904 ctermfg=1
hi diffNewFile gui=none guifg=#30aeb0 ctermfg=4
hi diffFile gui=none guifg=#7fad00 cterm=none ctermfg=3

hi GitGutterAdd guibg=#283431 guifg=#34b534 ctermbg=8 ctermfg=2
hi GitGutterChange gui=bold guibg=#283431 guifg=#ab9aa9 cterm=bold ctermbg=8 ctermfg=7
hi GitGutterDelete guibg=#283431 guifg=#c19904 ctermbg=8 ctermfg=1
hi GitGutterChangeDelete gui=bold guibg=#283431 guifg=#c19904 cterm=bold ctermbg=8 ctermfg=1

" Neomake
" -------
hi NeomakeError gui=none guibg=#c19904 guifg=#162c22 cterm=none ctermbg=1 ctermfg=0
hi NeomakeInfo gui=none guibg=#5fad8f guifg=#162c22 cterm=none ctermbg=6 ctermfg=0
hi NeomakeWarning gui=none guibg=#7fad00 guifg=#162c22 cterm=none ctermbg=3 ctermfg=0
hi NeomakeMessage gui=none guibg=#82bd00 guifg=#162c22 cterm=none ctermbg=11 ctermfg=0

hi NeomakeVirtualtextInfoDefault guifg=#10c480 ctermfg=14
hi NeomakeVirtualtextMessageDefault guifg=#82bd00 ctermfg=11
hi NeomakeVirtualtextWarningDefault guifg=#7fad00 ctermfg=3
hi NeomakeVirtualtextErrorDefault guifg=#c19904 ctermfg=1

hi NeomakeStatusGood gui=none guibg=#34b534 guifg=#162c22 cterm=none ctermbg=2 ctermfg=0
hi NeomakeStatusGoodNC gui=none guibg=#283431 guifg=#34b534 cterm=none ctermbg=8 ctermfg=2

hi NeomakeStatColorDefault gui=none guibg=#30aeb0 guifg=#162c22 cterm=none ctermbg=4 ctermfg=0
hi NeomakeStatColorTypeE gui=none guibg=#c19904 guifg=#162c22 cterm=none ctermbg=1 ctermfg=0
hi NeomakeStatColorTypeW gui=none guibg=#7fad00 guifg=#162c22 cterm=none ctermbg=3 ctermfg=0

" Markdown
" --------
hi MarkdownRule gui=bold guibg=#283431 guifg=#6ac134 cterm=bold ctermbg=8 ctermfg=10

hi MarkdownHeading gui=bold guifg=#bbbcbc cterm=bold ctermfg=15
hi default link MarkdownH1 MarkdownHeading
hi default link MarkdownH2 MarkdownHeading
hi default link MarkdownH3 MarkdownHeading
hi default link MarkdownH4 MarkdownHeading
hi default link MarkdownH5 MarkdownHeading
hi default link MarkdownH6 MarkdownHeading
hi default link MarkdownHeadingDelimiter MarkdownHeading
hi default link MarkdownHeadingRule MarkdownHeading

hi MarkdownBold gui=bold guifg=#d2a634 cterm=bold ctermfg=9
hi default link MarkdownBoldDelimiter MarkdownBold

hi MarkdownItalic gui=italic guifg=#7fad00 cterm=none ctermfg=3
hi default link MarkdownItalicDelimiter MarkdownItalic

hi MarkdownUrl gui=underline guifg=#30aeb0 cterm=underline ctermfg=4
hi MarkdownLinkText gui=none guifg=#56bdad cterm=none ctermfg=12
hi MarkdownLinkDelimiter gui=bold guifg=#bbbcbc cterm=bold ctermfg=15
hi default link MarkdownLinkTextDelimiter MarkdownLinkDelimiter

hi MarkdownCode guifg=#c8954c ctermfg=5
hi default link MarkdownCodeDelimiter MarkdownCode

hi MarkdownCodeBlock guifg=#bbbcbc ctermfg=15

hi MarkdownListMarker gui=none guifg=#34b534 cterm=none ctermfg=2
hi default link MarkdownOrderedListMarker MarkdownListMarker

" Linting
" -------
hi YcmErrorSection gui=undercurl guisp=#c19904 cterm=underline
hi YcmWarningSection gui=undercurl guisp=#7fad00 cterm=underline
hi SyntasticError gui=undercurl guisp=#c19904 cterm=underline
hi SyntasticWarning gui=undercurl guisp=#7fad00 cterm=underline
hi SyntasticErrorSing guifg=#162c22 guibg=#c19904 ctermfg=0 ctermbg=1
hi SyntasticWarningSign guifg=#162c22 guibg=#7fad00 ctermfg=0 ctermbg=3
