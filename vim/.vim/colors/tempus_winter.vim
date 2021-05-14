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
if exists("g:tempus_enforce_background_color")
	hi Normal guibg=#202427 guifg=#8da3b8 ctermbg=0 ctermfg=15
else
	" NOTE the ctermbg=none is for terminals with transparency
	hi Normal guibg=#202427 guifg=#8da3b8 ctermbg=none ctermfg=15
endif

hi Visual guibg=#8da3b8 guifg=#202427 ctermbg=15 ctermfg=0
hi Search gui=underline,bold guibg=#2a2e38 guifg=#8da3b8 cterm=underline,bold ctermbg=8 ctermfg=3
hi IncSearch gui=underline,bold guibg=#91959b guifg=#202427 term=none cterm=underline,bold ctermbg=7 ctermfg=0

hi StatusLine gui=none guibg=#8da3b8 guifg=#202427 cterm=none ctermbg=15 ctermfg=0
hi StatusLineNC gui=none guibg=#2a2e38 guifg=#91959b cterm=none ctermbg=8 ctermfg=7
hi StatusLineTerm gui=none guibg=#4aa920 guifg=#202427 cterm=none ctermbg=2 ctermfg=0
hi StatusLineTermNC gui=none guibg=#2a2e38 guifg=#4aa920 cterm=none ctermbg=8 ctermfg=2

hi VertSplit gui=none cterm=none
hi TabLine gui=none guibg=#2a2e38 guifg=#91959b cterm=none ctermbg=8 ctermfg=7
hi TabLineSel gui=none guibg=#4fa394 guifg=#202427 cterm=none ctermbg=6 ctermfg=0
hi TabLineFill gui=none cterm=none

hi Comment gui=italic guifg=#91959b cterm=none ctermfg=7
hi Todo gui=bold guibg=#2a2e38 guifg=#af9155 cterm=bold ctermbg=8 ctermfg=11

hi Warning gui=none guibg=#9a9921 guifg=#202427 cterm=none ctermbg=3 ctermfg=0
hi WarningMsg gui=none guibg=#9a9921 guifg=#202427 cterm=none ctermbg=3 ctermfg=0
hi Error gui=none guibg=#ed6e5a guifg=#202427 cterm=none ctermbg=1 ctermfg=0
hi ErrorMsg gui=none guibg=#ed6e5a guifg=#202427 cterm=none ctermbg=1 ctermfg=0

hi MatchParen gui=underline,bold guibg=#2a2e38 guifg=#91959b cterm=underline,bold ctermbg=8 ctermfg=7

hi ToolbarLine guibg=#91959b guifg=#202427 term=none ctermbg=7 ctermfg=0
hi ToolbarButton gui=bold guibg=#91959b guifg=#202427 term=none cterm=bold ctermbg=7 ctermfg=0

hi WildMenu guibg=#202427 guifg=#8da3b8 term=standout ctermbg=0 ctermfg=15

hi Terminal guibg=#202427 guifg=#8da3b8 term=none ctermbg=0 ctermfg=15

" Constructs
" ----------
hi Constant guifg=#7b91df ctermfg=4
hi Number guifg=#7b91df ctermfg=4
hi Float guifg=#7b91df ctermfg=4
hi String guifg=#329fcb ctermfg=12

hi Function guifg=#d17e80 ctermfg=5
hi Identifier guifg=#ca77c5 term=none ctermfg=13
hi Label guifg=#d17e80 ctermfg=5
hi Tag guifg=#d17e80 ctermfg=5
hi Keyword gui=bold guifg=#ca77c5 gui=bold ctermfg=13

hi Character gui=bold guifg=#1ba6a4 cterm=bold ctermfg=14

hi Type gui=none,bold guifg=#4fa394 term=none cterm=none,bold ctermfg=6
hi Boolean guifg=#4fa394 ctermfg=6
hi StorageClass guifg=#4fa394 ctermfg=6
hi Structure guifg=#4fa394 ctermfg=6
hi Typedef gui=bold guifg=#1ba6a4 cterm=bold ctermfg=14

hi Conditional gui=bold guifg=#4aa920 cterm=bold ctermfg=2
hi Statement gui=none guifg=#00ab5f cterm=none ctermfg=10
hi Repeat gui=bold guifg=#00ab5f cterm=bold ctermfg=10
hi Operator gui=bold guifg=#8da3b8 cterm=bold ctermfg=15
hi Exception gui=bold guifg=#ed6e5a cterm=bold ctermfg=1

hi Preproc gui=none guifg=#de7b28 term=none cterm=none ctermfg=9
hi PreCondit gui=bold guifg=#de7b28 cterm=bold ctermfg=9
hi Macro gui=bold guifg=#de7b28 cterm=bold ctermfg=9
hi Include guifg=#de7b28 ctermfg=9
hi Define guifg=#de7b28 ctermfg=9

hi Title gui=bold guibg=#202427 guifg=#4fa394 cterm=bold ctermbg=0 ctermfg=6

hi Delimeter gui=bold guifg=#d17e80 cterm=bold ctermfg=5
hi Delimiter gui=bold guifg=#d17e80 cterm=bold ctermfg=5
hi SpecialComment gui=bold guifg=#d17e80 cterm=bold ctermfg=5

hi Debug guifg=#ca77c5 ctermfg=13

" Other
" -----
hi LineNr guibg=#2a2e38 guifg=#91959b term=none ctermbg=8 ctermfg=7
hi Cursor guibg=#8da3b8 guifg=#202427 ctermbg=15 ctermfg=0
hi CursorLine gui=none guibg=#2a2e38 term=none cterm=none ctermbg=8
hi CursorColumn gui=none guibg=#2a2e38 term=none cterm=none ctermbg=8
hi CursorLineNr gui=bold guibg=#91959b guifg=#202427 cterm=bold ctermbg=7 ctermfg=0
hi ColorColumn guibg=#2a2e38 guifg=#8da3b8 term=none ctermbg=8 ctermfg=15
hi SignColumn guibg=#2a2e38 guifg=#91959b term=none ctermbg=8 ctermfg=7

hi Folded guibg=#2a2e38 guifg=#91959b ctermbg=8 ctermfg=7
hi FoldColumn guibg=#2a2e38 guifg=#91959b ctermbg=8 ctermfg=7

hi Special gui=bold guifg=#af9155 term=none cterm=bold ctermfg=11
hi SpecialKey gui=none guibg=#2a2e38 guifg=#91959b cterm=none ctermbg=8 ctermfg=7
hi SpecialChar gui=bold guifg=#af9155 cterm=bold ctermfg=11
hi NonText gui=none guibg=#2a2e38 guifg=#91959b cterm=none ctermbg=8 ctermfg=7
hi EndOfBuffer gui=bold guifg=#91959b cterm=bold ctermfg=7

hi Directory gui=none guifg=#4aa920 term=none cterm=none ctermfg=2
hi Question gui=bold guifg=#af9155 cterm=bold ctermfg=11
hi MoreMsg guifg=#00ab5f ctermfg=10
hi ModeMsg gui=bold guifg=#4aa920 cterm=bold ctermfg=2

hi VimOption guifg=#d17e80 ctermfg=5
hi VimGroup guifg=#d17e80 ctermfg=5

hi Underlined gui=underline guifg=#8da3b8 cterm=underline ctermfg=15
hi Ignore guibg=#2a2e38 guifg=#91959b ctermbg=8 ctermfg=7
hi Conceal guibg=#91959b guifg=#2a2e38 ctermbg=7 ctermfg=8

hi SpellBad guibg=#ed6e5a guifg=#202427 ctermbg=1 ctermfg=0
hi SpellCap guibg=#9a9921 guifg=#202427 ctermbg=3 ctermfg=0
hi SpellRare guibg=#ca77c5 guifg=#202427 ctermbg=13 ctermfg=0
hi SpellLocal guibg=#1ba6a4 guifg=#202427 ctermbg=14 ctermfg=0

hi Pmenu gui=italic guibg=#2a2e38 guifg=#8da3b8 cterm=none ctermbg=8 ctermfg=15
hi PmenuSel gui=none,bold guibg=#91959b guifg=#202427 cterm=none,bold ctermbg=7 ctermfg=0
hi PmenuSbar guibg=#2a2e38 ctermbg=8
hi PmenuThumb guibg=#91959b ctermbg=7

" Terminal
" --------

if exists('*term_setansicolors')
	let g:terminal_ansi_colors = [
				\ "#202427",
				\ "#ed6e5a",
				\ "#4aa920",
				\ "#9a9921",
				\ "#7b91df",
				\ "#d17e80",
				\ "#4fa394",
				\ "#91959b",
				\ "#2a2e38",
				\ "#de7b28",
				\ "#00ab5f",
				\ "#af9155",
				\ "#329fcb",
				\ "#ca77c5",
				\ "#1ba6a4",
				\ "#8da3b8"
				\ ]
endif
if has('nvim')
	let g:terminal_color_0 = "#202427"
	let g:terminal_color_1 = "#ed6e5a"
	let g:terminal_color_2 = "#4aa920"
	let g:terminal_color_3 = "#9a9921"
	let g:terminal_color_4 = "#7b91df"
	let g:terminal_color_5 = "#d17e80"
	let g:terminal_color_6 = "#4fa394"
	let g:terminal_color_7 = "#91959b"
	let g:terminal_color_8 = "#2a2e38"
	let g:terminal_color_9 = "#de7b28"
	let g:terminal_color_10 = "#00ab5f"
	let g:terminal_color_11 = "#af9155"
	let g:terminal_color_12 = "#329fcb"
	let g:terminal_color_13 = "#ca77c5"
	let g:terminal_color_14 = "#1ba6a4"
	let g:terminal_color_15 = "#8da3b8"
endif

" Diffs
" -----
hi DiffAdd gui=bold guibg=#4aa920 guifg=#202427 cterm=bold ctermbg=10 ctermfg=0
hi DiffDelete gui=none guibg=#ed6e5a guifg=#202427 cterm=none ctermbg=9 ctermfg=0
hi DiffChange gui=bold guibg=#2a2e38 guifg=#91959b cterm=bold ctermbg=8 ctermfg=7
hi DiffText gui=bold guibg=#2a2e38 guifg=#de7b28 cterm=bold ctermbg=8 ctermfg=1

hi diffAdded guifg=#4aa920 ctermfg=2
hi diffRemoved guifg=#ed6e5a ctermfg=1
hi diffNewFile gui=none guifg=#7b91df ctermfg=4
hi diffFile gui=none guifg=#9a9921 cterm=none ctermfg=3

hi GitGutterAdd guibg=#2a2e38 guifg=#4aa920 ctermbg=8 ctermfg=2
hi GitGutterChange gui=bold guibg=#2a2e38 guifg=#91959b cterm=bold ctermbg=8 ctermfg=7
hi GitGutterDelete guibg=#2a2e38 guifg=#ed6e5a ctermbg=8 ctermfg=1
hi GitGutterChangeDelete gui=bold guibg=#2a2e38 guifg=#ed6e5a cterm=bold ctermbg=8 ctermfg=1

" Neomake
" -------
hi NeomakeError gui=none guibg=#ed6e5a guifg=#202427 cterm=none ctermbg=1 ctermfg=0
hi NeomakeInfo gui=none guibg=#4fa394 guifg=#202427 cterm=none ctermbg=6 ctermfg=0
hi NeomakeWarning gui=none guibg=#9a9921 guifg=#202427 cterm=none ctermbg=3 ctermfg=0
hi NeomakeMessage gui=none guibg=#af9155 guifg=#202427 cterm=none ctermbg=11 ctermfg=0

hi NeomakeVirtualtextInfoDefault guifg=#1ba6a4 ctermfg=14
hi NeomakeVirtualtextMessageDefault guifg=#af9155 ctermfg=11
hi NeomakeVirtualtextWarningDefault guifg=#9a9921 ctermfg=3
hi NeomakeVirtualtextErrorDefault guifg=#ed6e5a ctermfg=1

hi NeomakeStatusGood gui=none guibg=#4aa920 guifg=#202427 cterm=none ctermbg=2 ctermfg=0
hi NeomakeStatusGoodNC gui=none guibg=#2a2e38 guifg=#4aa920 cterm=none ctermbg=8 ctermfg=2

hi NeomakeStatColorDefault gui=none guibg=#7b91df guifg=#202427 cterm=none ctermbg=4 ctermfg=0
hi NeomakeStatColorTypeE gui=none guibg=#ed6e5a guifg=#202427 cterm=none ctermbg=1 ctermfg=0
hi NeomakeStatColorTypeW gui=none guibg=#9a9921 guifg=#202427 cterm=none ctermbg=3 ctermfg=0

" Markdown
" --------
hi MarkdownRule gui=bold guibg=#2a2e38 guifg=#00ab5f cterm=bold ctermbg=8 ctermfg=10

hi MarkdownHeading gui=bold guifg=#8da3b8 cterm=bold ctermfg=15
hi default link MarkdownH1 MarkdownHeading
hi default link MarkdownH2 MarkdownHeading
hi default link MarkdownH3 MarkdownHeading
hi default link MarkdownH4 MarkdownHeading
hi default link MarkdownH5 MarkdownHeading
hi default link MarkdownH6 MarkdownHeading
hi default link MarkdownHeadingDelimiter MarkdownHeading
hi default link MarkdownHeadingRule MarkdownHeading

hi MarkdownBold gui=bold guifg=#de7b28 cterm=bold ctermfg=9
hi default link MarkdownBoldDelimiter MarkdownBold

hi MarkdownItalic gui=italic guifg=#9a9921 cterm=none ctermfg=3
hi default link MarkdownItalicDelimiter MarkdownItalic

hi MarkdownUrl gui=underline guifg=#7b91df cterm=underline ctermfg=4
hi MarkdownLinkText gui=none guifg=#329fcb cterm=none ctermfg=12
hi MarkdownLinkDelimiter gui=bold guifg=#8da3b8 cterm=bold ctermfg=15
hi default link MarkdownLinkTextDelimiter MarkdownLinkDelimiter

hi MarkdownCode guifg=#d17e80 ctermfg=5
hi default link MarkdownCodeDelimiter MarkdownCode

hi MarkdownCodeBlock guifg=#8da3b8 ctermfg=15

hi MarkdownListMarker gui=none guifg=#4aa920 cterm=none ctermfg=2
hi default link MarkdownOrderedListMarker MarkdownListMarker

" Linting
" -------
hi YcmErrorSection gui=undercurl guisp=#ed6e5a cterm=underline
hi YcmWarningSection gui=undercurl guisp=#9a9921 cterm=underline
hi SyntasticError gui=undercurl guisp=#ed6e5a cterm=underline
hi SyntasticWarning gui=undercurl guisp=#9a9921 cterm=underline
hi SyntasticErrorSing guifg=#202427 guibg=#ed6e5a ctermfg=0 ctermbg=1
hi SyntasticWarningSign guifg=#202427 guibg=#9a9921 ctermfg=0 ctermbg=3
