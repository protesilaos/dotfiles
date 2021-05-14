" Name: Tempus Night
" Description: High contrast dark theme with bright colours (WCAG AAA compliant)
" Author: Protesilaos Stavrou (https://protesilaos.com)
" Meta: Created with the Tempus Themes Generator
" URL: https://gitlab.com/protesilaos/tempus-themes-generator

set background=dark
hi clear
if exists("syntax_on")
	syntax reset
endif
let g:colors_name = "tempus_night"

" General
" -------
if exists("g:tempus_enforce_background_color")
	hi Normal guibg=#1a1a1a guifg=#e0e0e0 ctermbg=0 ctermfg=15
else
	" NOTE the ctermbg=none is for terminals with transparency
	hi Normal guibg=#1a1a1a guifg=#e0e0e0 ctermbg=none ctermfg=15
endif

hi Visual guibg=#e0e0e0 guifg=#1a1a1a ctermbg=15 ctermfg=0
hi Search gui=underline,bold guibg=#242536 guifg=#e0e0e0 cterm=underline,bold ctermbg=8 ctermfg=3
hi IncSearch gui=underline,bold guibg=#c4bdaf guifg=#1a1a1a term=none cterm=underline,bold ctermbg=7 ctermfg=0

hi StatusLine gui=none guibg=#e0e0e0 guifg=#1a1a1a cterm=none ctermbg=15 ctermfg=0
hi StatusLineNC gui=none guibg=#242536 guifg=#c4bdaf cterm=none ctermbg=8 ctermfg=7
hi StatusLineTerm gui=none guibg=#5fc940 guifg=#1a1a1a cterm=none ctermbg=2 ctermfg=0
hi StatusLineTermNC gui=none guibg=#242536 guifg=#5fc940 cterm=none ctermbg=8 ctermfg=2

hi VertSplit gui=none cterm=none
hi TabLine gui=none guibg=#242536 guifg=#c4bdaf cterm=none ctermbg=8 ctermfg=7
hi TabLineSel gui=none guibg=#1dc5c3 guifg=#1a1a1a cterm=none ctermbg=6 ctermfg=0
hi TabLineFill gui=none cterm=none

hi Comment gui=italic guifg=#c4bdaf cterm=none ctermfg=7
hi Todo gui=bold guibg=#242536 guifg=#d7ae00 cterm=bold ctermbg=8 ctermfg=11

hi Warning gui=none guibg=#c5b300 guifg=#1a1a1a cterm=none ctermbg=3 ctermfg=0
hi WarningMsg gui=none guibg=#c5b300 guifg=#1a1a1a cterm=none ctermbg=3 ctermfg=0
hi Error gui=none guibg=#ff929f guifg=#1a1a1a cterm=none ctermbg=1 ctermfg=0
hi ErrorMsg gui=none guibg=#ff929f guifg=#1a1a1a cterm=none ctermbg=1 ctermfg=0

hi MatchParen gui=underline,bold guibg=#242536 guifg=#c4bdaf cterm=underline,bold ctermbg=8 ctermfg=7

hi ToolbarLine guibg=#c4bdaf guifg=#1a1a1a term=none ctermbg=7 ctermfg=0
hi ToolbarButton gui=bold guibg=#c4bdaf guifg=#1a1a1a term=none cterm=bold ctermbg=7 ctermfg=0

hi WildMenu guibg=#1a1a1a guifg=#e0e0e0 term=standout ctermbg=0 ctermfg=15

hi Terminal guibg=#1a1a1a guifg=#e0e0e0 term=none ctermbg=0 ctermfg=15

" Constructs
" ----------
hi Constant guifg=#5fb8ff ctermfg=4
hi Number guifg=#5fb8ff ctermfg=4
hi Float guifg=#5fb8ff ctermfg=4
hi String guifg=#8cb4f0 ctermfg=12

hi Function guifg=#ef91df ctermfg=5
hi Identifier guifg=#de99f0 term=none ctermfg=13
hi Label guifg=#ef91df ctermfg=5
hi Tag guifg=#ef91df ctermfg=5
hi Keyword gui=bold guifg=#de99f0 gui=bold ctermfg=13

hi Character gui=bold guifg=#00ca9a cterm=bold ctermfg=14

hi Type gui=none,bold guifg=#1dc5c3 term=none cterm=none,bold ctermfg=6
hi Boolean guifg=#1dc5c3 ctermfg=6
hi StorageClass guifg=#1dc5c3 ctermfg=6
hi Structure guifg=#1dc5c3 ctermfg=6
hi Typedef gui=bold guifg=#00ca9a cterm=bold ctermfg=14

hi Conditional gui=bold guifg=#5fc940 cterm=bold ctermfg=2
hi Statement gui=none guifg=#88c400 cterm=none ctermfg=10
hi Repeat gui=bold guifg=#88c400 cterm=bold ctermfg=10
hi Operator gui=bold guifg=#e0e0e0 cterm=bold ctermfg=15
hi Exception gui=bold guifg=#ff929f cterm=bold ctermfg=1

hi Preproc gui=none guifg=#f69d6a term=none cterm=none ctermfg=9
hi PreCondit gui=bold guifg=#f69d6a cterm=bold ctermfg=9
hi Macro gui=bold guifg=#f69d6a cterm=bold ctermfg=9
hi Include guifg=#f69d6a ctermfg=9
hi Define guifg=#f69d6a ctermfg=9

hi Title gui=bold guibg=#1a1a1a guifg=#1dc5c3 cterm=bold ctermbg=0 ctermfg=6

hi Delimeter gui=bold guifg=#ef91df cterm=bold ctermfg=5
hi Delimiter gui=bold guifg=#ef91df cterm=bold ctermfg=5
hi SpecialComment gui=bold guifg=#ef91df cterm=bold ctermfg=5

hi Debug guifg=#de99f0 ctermfg=13

" Other
" -----
hi LineNr guibg=#242536 guifg=#c4bdaf term=none ctermbg=8 ctermfg=7
hi Cursor guibg=#e0e0e0 guifg=#1a1a1a ctermbg=15 ctermfg=0
hi CursorLine gui=none guibg=#242536 term=none cterm=none ctermbg=8
hi CursorColumn gui=none guibg=#242536 term=none cterm=none ctermbg=8
hi CursorLineNr gui=bold guibg=#c4bdaf guifg=#1a1a1a cterm=bold ctermbg=7 ctermfg=0
hi ColorColumn guibg=#242536 guifg=#e0e0e0 term=none ctermbg=8 ctermfg=15
hi SignColumn guibg=#242536 guifg=#c4bdaf term=none ctermbg=8 ctermfg=7

hi Folded guibg=#242536 guifg=#c4bdaf ctermbg=8 ctermfg=7
hi FoldColumn guibg=#242536 guifg=#c4bdaf ctermbg=8 ctermfg=7

hi Special gui=bold guifg=#d7ae00 term=none cterm=bold ctermfg=11
hi SpecialKey gui=none guibg=#242536 guifg=#c4bdaf cterm=none ctermbg=8 ctermfg=7
hi SpecialChar gui=bold guifg=#d7ae00 cterm=bold ctermfg=11
hi NonText gui=none guibg=#242536 guifg=#c4bdaf cterm=none ctermbg=8 ctermfg=7
hi EndOfBuffer gui=bold guifg=#c4bdaf cterm=bold ctermfg=7

hi Directory gui=none guifg=#5fc940 term=none cterm=none ctermfg=2
hi Question gui=bold guifg=#d7ae00 cterm=bold ctermfg=11
hi MoreMsg guifg=#88c400 ctermfg=10
hi ModeMsg gui=bold guifg=#5fc940 cterm=bold ctermfg=2

hi VimOption guifg=#ef91df ctermfg=5
hi VimGroup guifg=#ef91df ctermfg=5

hi Underlined gui=underline guifg=#e0e0e0 cterm=underline ctermfg=15
hi Ignore guibg=#242536 guifg=#c4bdaf ctermbg=8 ctermfg=7
hi Conceal guibg=#c4bdaf guifg=#242536 ctermbg=7 ctermfg=8

hi SpellBad guibg=#ff929f guifg=#1a1a1a ctermbg=1 ctermfg=0
hi SpellCap guibg=#c5b300 guifg=#1a1a1a ctermbg=3 ctermfg=0
hi SpellRare guibg=#de99f0 guifg=#1a1a1a ctermbg=13 ctermfg=0
hi SpellLocal guibg=#00ca9a guifg=#1a1a1a ctermbg=14 ctermfg=0

hi Pmenu gui=italic guibg=#242536 guifg=#e0e0e0 cterm=none ctermbg=8 ctermfg=15
hi PmenuSel gui=none,bold guibg=#c4bdaf guifg=#1a1a1a cterm=none,bold ctermbg=7 ctermfg=0
hi PmenuSbar guibg=#242536 ctermbg=8
hi PmenuThumb guibg=#c4bdaf ctermbg=7

" Terminal
" --------

if exists('*term_setansicolors')
	let g:terminal_ansi_colors = [
				\ "#1a1a1a",
				\ "#ff929f",
				\ "#5fc940",
				\ "#c5b300",
				\ "#5fb8ff",
				\ "#ef91df",
				\ "#1dc5c3",
				\ "#c4bdaf",
				\ "#242536",
				\ "#f69d6a",
				\ "#88c400",
				\ "#d7ae00",
				\ "#8cb4f0",
				\ "#de99f0",
				\ "#00ca9a",
				\ "#e0e0e0"
				\ ]
endif
if has('nvim')
	let g:terminal_color_0 = "#1a1a1a"
	let g:terminal_color_1 = "#ff929f"
	let g:terminal_color_2 = "#5fc940"
	let g:terminal_color_3 = "#c5b300"
	let g:terminal_color_4 = "#5fb8ff"
	let g:terminal_color_5 = "#ef91df"
	let g:terminal_color_6 = "#1dc5c3"
	let g:terminal_color_7 = "#c4bdaf"
	let g:terminal_color_8 = "#242536"
	let g:terminal_color_9 = "#f69d6a"
	let g:terminal_color_10 = "#88c400"
	let g:terminal_color_11 = "#d7ae00"
	let g:terminal_color_12 = "#8cb4f0"
	let g:terminal_color_13 = "#de99f0"
	let g:terminal_color_14 = "#00ca9a"
	let g:terminal_color_15 = "#e0e0e0"
endif

" Diffs
" -----
hi DiffAdd gui=bold guibg=#5fc940 guifg=#1a1a1a cterm=bold ctermbg=10 ctermfg=0
hi DiffDelete gui=none guibg=#ff929f guifg=#1a1a1a cterm=none ctermbg=9 ctermfg=0
hi DiffChange gui=bold guibg=#242536 guifg=#c4bdaf cterm=bold ctermbg=8 ctermfg=7
hi DiffText gui=bold guibg=#242536 guifg=#f69d6a cterm=bold ctermbg=8 ctermfg=1

hi diffAdded guifg=#5fc940 ctermfg=2
hi diffRemoved guifg=#ff929f ctermfg=1
hi diffNewFile gui=none guifg=#5fb8ff ctermfg=4
hi diffFile gui=none guifg=#c5b300 cterm=none ctermfg=3

hi GitGutterAdd guibg=#242536 guifg=#5fc940 ctermbg=8 ctermfg=2
hi GitGutterChange gui=bold guibg=#242536 guifg=#c4bdaf cterm=bold ctermbg=8 ctermfg=7
hi GitGutterDelete guibg=#242536 guifg=#ff929f ctermbg=8 ctermfg=1
hi GitGutterChangeDelete gui=bold guibg=#242536 guifg=#ff929f cterm=bold ctermbg=8 ctermfg=1

" Neomake
" -------
hi NeomakeError gui=none guibg=#ff929f guifg=#1a1a1a cterm=none ctermbg=1 ctermfg=0
hi NeomakeInfo gui=none guibg=#1dc5c3 guifg=#1a1a1a cterm=none ctermbg=6 ctermfg=0
hi NeomakeWarning gui=none guibg=#c5b300 guifg=#1a1a1a cterm=none ctermbg=3 ctermfg=0
hi NeomakeMessage gui=none guibg=#d7ae00 guifg=#1a1a1a cterm=none ctermbg=11 ctermfg=0

hi NeomakeVirtualtextInfoDefault guifg=#00ca9a ctermfg=14
hi NeomakeVirtualtextMessageDefault guifg=#d7ae00 ctermfg=11
hi NeomakeVirtualtextWarningDefault guifg=#c5b300 ctermfg=3
hi NeomakeVirtualtextErrorDefault guifg=#ff929f ctermfg=1

hi NeomakeStatusGood gui=none guibg=#5fc940 guifg=#1a1a1a cterm=none ctermbg=2 ctermfg=0
hi NeomakeStatusGoodNC gui=none guibg=#242536 guifg=#5fc940 cterm=none ctermbg=8 ctermfg=2

hi NeomakeStatColorDefault gui=none guibg=#5fb8ff guifg=#1a1a1a cterm=none ctermbg=4 ctermfg=0
hi NeomakeStatColorTypeE gui=none guibg=#ff929f guifg=#1a1a1a cterm=none ctermbg=1 ctermfg=0
hi NeomakeStatColorTypeW gui=none guibg=#c5b300 guifg=#1a1a1a cterm=none ctermbg=3 ctermfg=0

" Markdown
" --------
hi MarkdownRule gui=bold guibg=#242536 guifg=#88c400 cterm=bold ctermbg=8 ctermfg=10

hi MarkdownHeading gui=bold guifg=#e0e0e0 cterm=bold ctermfg=15
hi default link MarkdownH1 MarkdownHeading
hi default link MarkdownH2 MarkdownHeading
hi default link MarkdownH3 MarkdownHeading
hi default link MarkdownH4 MarkdownHeading
hi default link MarkdownH5 MarkdownHeading
hi default link MarkdownH6 MarkdownHeading
hi default link MarkdownHeadingDelimiter MarkdownHeading
hi default link MarkdownHeadingRule MarkdownHeading

hi MarkdownBold gui=bold guifg=#f69d6a cterm=bold ctermfg=9
hi default link MarkdownBoldDelimiter MarkdownBold

hi MarkdownItalic gui=italic guifg=#c5b300 cterm=none ctermfg=3
hi default link MarkdownItalicDelimiter MarkdownItalic

hi MarkdownUrl gui=underline guifg=#5fb8ff cterm=underline ctermfg=4
hi MarkdownLinkText gui=none guifg=#8cb4f0 cterm=none ctermfg=12
hi MarkdownLinkDelimiter gui=bold guifg=#e0e0e0 cterm=bold ctermfg=15
hi default link MarkdownLinkTextDelimiter MarkdownLinkDelimiter

hi MarkdownCode guifg=#ef91df ctermfg=5
hi default link MarkdownCodeDelimiter MarkdownCode

hi MarkdownCodeBlock guifg=#e0e0e0 ctermfg=15

hi MarkdownListMarker gui=none guifg=#5fc940 cterm=none ctermfg=2
hi default link MarkdownOrderedListMarker MarkdownListMarker

" Linting
" -------
hi YcmErrorSection gui=undercurl guisp=#ff929f cterm=underline
hi YcmWarningSection gui=undercurl guisp=#c5b300 cterm=underline
hi SyntasticError gui=undercurl guisp=#ff929f cterm=underline
hi SyntasticWarning gui=undercurl guisp=#c5b300 cterm=underline
hi SyntasticErrorSing guifg=#1a1a1a guibg=#ff929f ctermfg=0 ctermbg=1
hi SyntasticWarningSign guifg=#1a1a1a guibg=#c5b300 ctermfg=0 ctermbg=3
