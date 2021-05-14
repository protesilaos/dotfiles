" Name: Tempus Autumn
" Description: Dark theme with a palette inspired by earthly colours (WCAG AA compliant)
" Author: Protesilaos Stavrou (https://protesilaos.com)
" Meta: Created with the Tempus Themes Generator
" URL: https://gitlab.com/protesilaos/tempus-themes-generator

set background=dark
hi clear
if exists("syntax_on")
	syntax reset
endif
let g:colors_name = "tempus_autumn"

" General
" -------
if exists("g:tempus_enforce_background_color")
	hi Normal guibg=#302420 guifg=#a9a2a6 ctermbg=0 ctermfg=15
else
	" NOTE the ctermbg=none is for terminals with transparency
	hi Normal guibg=#302420 guifg=#a9a2a6 ctermbg=none ctermfg=15
endif

hi Visual guibg=#a9a2a6 guifg=#302420 ctermbg=15 ctermfg=0
hi Search gui=underline,bold guibg=#36302a guifg=#a9a2a6 cterm=underline,bold ctermbg=8 ctermfg=3
hi IncSearch gui=underline,bold guibg=#a8948a guifg=#302420 term=none cterm=underline,bold ctermbg=7 ctermfg=0

hi StatusLine gui=none guibg=#a9a2a6 guifg=#302420 cterm=none ctermbg=15 ctermfg=0
hi StatusLineNC gui=none guibg=#36302a guifg=#a8948a cterm=none ctermbg=8 ctermfg=7
hi StatusLineTerm gui=none guibg=#85a400 guifg=#302420 cterm=none ctermbg=2 ctermfg=0
hi StatusLineTermNC gui=none guibg=#36302a guifg=#85a400 cterm=none ctermbg=8 ctermfg=2

hi VertSplit gui=none cterm=none
hi TabLine gui=none guibg=#36302a guifg=#a8948a cterm=none ctermbg=8 ctermfg=7
hi TabLineSel gui=none guibg=#52a885 guifg=#302420 cterm=none ctermbg=6 ctermfg=0
hi TabLineFill gui=none cterm=none

hi Comment gui=italic guifg=#a8948a cterm=none ctermfg=7
hi Todo gui=bold guibg=#36302a guifg=#ba9400 cterm=bold ctermbg=8 ctermfg=11

hi Warning gui=none guibg=#b09640 guifg=#302420 cterm=none ctermbg=3 ctermfg=0
hi WarningMsg gui=none guibg=#b09640 guifg=#302420 cterm=none ctermbg=3 ctermfg=0
hi Error gui=none guibg=#f46f55 guifg=#302420 cterm=none ctermbg=1 ctermfg=0
hi ErrorMsg gui=none guibg=#f46f55 guifg=#302420 cterm=none ctermbg=1 ctermfg=0

hi MatchParen gui=underline,bold guibg=#36302a guifg=#a8948a cterm=underline,bold ctermbg=8 ctermfg=7

hi ToolbarLine guibg=#a8948a guifg=#302420 term=none ctermbg=7 ctermfg=0
hi ToolbarButton gui=bold guibg=#a8948a guifg=#302420 term=none cterm=bold ctermbg=7 ctermfg=0

hi WildMenu guibg=#302420 guifg=#a9a2a6 term=standout ctermbg=0 ctermfg=15

hi Terminal guibg=#302420 guifg=#a9a2a6 term=none ctermbg=0 ctermfg=15

" Constructs
" ----------
hi Constant guifg=#799aca ctermfg=4
hi Number guifg=#799aca ctermfg=4
hi Float guifg=#799aca ctermfg=4
hi String guifg=#958fdf ctermfg=12

hi Function guifg=#df798e ctermfg=5
hi Identifier guifg=#ce7dc4 term=none ctermfg=13
hi Label guifg=#df798e ctermfg=5
hi Tag guifg=#df798e ctermfg=5
hi Keyword gui=bold guifg=#ce7dc4 gui=bold ctermfg=13

hi Character gui=bold guifg=#2fa6b7 cterm=bold ctermfg=14

hi Type gui=none,bold guifg=#52a885 term=none cterm=none,bold ctermfg=6
hi Boolean guifg=#52a885 ctermfg=6
hi StorageClass guifg=#52a885 ctermfg=6
hi Structure guifg=#52a885 ctermfg=6
hi Typedef gui=bold guifg=#2fa6b7 cterm=bold ctermfg=14

hi Conditional gui=bold guifg=#85a400 cterm=bold ctermfg=2
hi Statement gui=none guifg=#43aa7a cterm=none ctermfg=10
hi Repeat gui=bold guifg=#43aa7a cterm=bold ctermfg=10
hi Operator gui=bold guifg=#a9a2a6 cterm=bold ctermfg=15
hi Exception gui=bold guifg=#f46f55 cterm=bold ctermfg=1

hi Preproc gui=none guifg=#e27e3d term=none cterm=none ctermfg=9
hi PreCondit gui=bold guifg=#e27e3d cterm=bold ctermfg=9
hi Macro gui=bold guifg=#e27e3d cterm=bold ctermfg=9
hi Include guifg=#e27e3d ctermfg=9
hi Define guifg=#e27e3d ctermfg=9

hi Title gui=bold guibg=#302420 guifg=#52a885 cterm=bold ctermbg=0 ctermfg=6

hi Delimeter gui=bold guifg=#df798e cterm=bold ctermfg=5
hi Delimiter gui=bold guifg=#df798e cterm=bold ctermfg=5
hi SpecialComment gui=bold guifg=#df798e cterm=bold ctermfg=5

hi Debug guifg=#ce7dc4 ctermfg=13

" Other
" -----
hi LineNr guibg=#36302a guifg=#a8948a term=none ctermbg=8 ctermfg=7
hi Cursor guibg=#a9a2a6 guifg=#302420 ctermbg=15 ctermfg=0
hi CursorLine gui=none guibg=#36302a term=none cterm=none ctermbg=8
hi CursorColumn gui=none guibg=#36302a term=none cterm=none ctermbg=8
hi CursorLineNr gui=bold guibg=#a8948a guifg=#302420 cterm=bold ctermbg=7 ctermfg=0
hi ColorColumn guibg=#36302a guifg=#a9a2a6 term=none ctermbg=8 ctermfg=15
hi SignColumn guibg=#36302a guifg=#a8948a term=none ctermbg=8 ctermfg=7

hi Folded guibg=#36302a guifg=#a8948a ctermbg=8 ctermfg=7
hi FoldColumn guibg=#36302a guifg=#a8948a ctermbg=8 ctermfg=7

hi Special gui=bold guifg=#ba9400 term=none cterm=bold ctermfg=11
hi SpecialKey gui=none guibg=#36302a guifg=#a8948a cterm=none ctermbg=8 ctermfg=7
hi SpecialChar gui=bold guifg=#ba9400 cterm=bold ctermfg=11
hi NonText gui=none guibg=#36302a guifg=#a8948a cterm=none ctermbg=8 ctermfg=7
hi EndOfBuffer gui=bold guifg=#a8948a cterm=bold ctermfg=7

hi Directory gui=none guifg=#85a400 term=none cterm=none ctermfg=2
hi Question gui=bold guifg=#ba9400 cterm=bold ctermfg=11
hi MoreMsg guifg=#43aa7a ctermfg=10
hi ModeMsg gui=bold guifg=#85a400 cterm=bold ctermfg=2

hi VimOption guifg=#df798e ctermfg=5
hi VimGroup guifg=#df798e ctermfg=5

hi Underlined gui=underline guifg=#a9a2a6 cterm=underline ctermfg=15
hi Ignore guibg=#36302a guifg=#a8948a ctermbg=8 ctermfg=7
hi Conceal guibg=#a8948a guifg=#36302a ctermbg=7 ctermfg=8

hi SpellBad guibg=#f46f55 guifg=#302420 ctermbg=1 ctermfg=0
hi SpellCap guibg=#b09640 guifg=#302420 ctermbg=3 ctermfg=0
hi SpellRare guibg=#ce7dc4 guifg=#302420 ctermbg=13 ctermfg=0
hi SpellLocal guibg=#2fa6b7 guifg=#302420 ctermbg=14 ctermfg=0

hi Pmenu gui=italic guibg=#36302a guifg=#a9a2a6 cterm=none ctermbg=8 ctermfg=15
hi PmenuSel gui=none,bold guibg=#a8948a guifg=#302420 cterm=none,bold ctermbg=7 ctermfg=0
hi PmenuSbar guibg=#36302a ctermbg=8
hi PmenuThumb guibg=#a8948a ctermbg=7

" Terminal
" --------

if exists('*term_setansicolors')
	let g:terminal_ansi_colors = [
				\ "#302420",
				\ "#f46f55",
				\ "#85a400",
				\ "#b09640",
				\ "#799aca",
				\ "#df798e",
				\ "#52a885",
				\ "#a8948a",
				\ "#36302a",
				\ "#e27e3d",
				\ "#43aa7a",
				\ "#ba9400",
				\ "#958fdf",
				\ "#ce7dc4",
				\ "#2fa6b7",
				\ "#a9a2a6"
				\ ]
endif
if has('nvim')
	let g:terminal_color_0 = "#302420"
	let g:terminal_color_1 = "#f46f55"
	let g:terminal_color_2 = "#85a400"
	let g:terminal_color_3 = "#b09640"
	let g:terminal_color_4 = "#799aca"
	let g:terminal_color_5 = "#df798e"
	let g:terminal_color_6 = "#52a885"
	let g:terminal_color_7 = "#a8948a"
	let g:terminal_color_8 = "#36302a"
	let g:terminal_color_9 = "#e27e3d"
	let g:terminal_color_10 = "#43aa7a"
	let g:terminal_color_11 = "#ba9400"
	let g:terminal_color_12 = "#958fdf"
	let g:terminal_color_13 = "#ce7dc4"
	let g:terminal_color_14 = "#2fa6b7"
	let g:terminal_color_15 = "#a9a2a6"
endif

" Diffs
" -----
hi DiffAdd gui=bold guibg=#85a400 guifg=#302420 cterm=bold ctermbg=10 ctermfg=0
hi DiffDelete gui=none guibg=#f46f55 guifg=#302420 cterm=none ctermbg=9 ctermfg=0
hi DiffChange gui=bold guibg=#36302a guifg=#a8948a cterm=bold ctermbg=8 ctermfg=7
hi DiffText gui=bold guibg=#36302a guifg=#e27e3d cterm=bold ctermbg=8 ctermfg=1

hi diffAdded guifg=#85a400 ctermfg=2
hi diffRemoved guifg=#f46f55 ctermfg=1
hi diffNewFile gui=none guifg=#799aca ctermfg=4
hi diffFile gui=none guifg=#b09640 cterm=none ctermfg=3

hi GitGutterAdd guibg=#36302a guifg=#85a400 ctermbg=8 ctermfg=2
hi GitGutterChange gui=bold guibg=#36302a guifg=#a8948a cterm=bold ctermbg=8 ctermfg=7
hi GitGutterDelete guibg=#36302a guifg=#f46f55 ctermbg=8 ctermfg=1
hi GitGutterChangeDelete gui=bold guibg=#36302a guifg=#f46f55 cterm=bold ctermbg=8 ctermfg=1

" Neomake
" -------
hi NeomakeError gui=none guibg=#f46f55 guifg=#302420 cterm=none ctermbg=1 ctermfg=0
hi NeomakeInfo gui=none guibg=#52a885 guifg=#302420 cterm=none ctermbg=6 ctermfg=0
hi NeomakeWarning gui=none guibg=#b09640 guifg=#302420 cterm=none ctermbg=3 ctermfg=0
hi NeomakeMessage gui=none guibg=#ba9400 guifg=#302420 cterm=none ctermbg=11 ctermfg=0

hi NeomakeVirtualtextInfoDefault guifg=#2fa6b7 ctermfg=14
hi NeomakeVirtualtextMessageDefault guifg=#ba9400 ctermfg=11
hi NeomakeVirtualtextWarningDefault guifg=#b09640 ctermfg=3
hi NeomakeVirtualtextErrorDefault guifg=#f46f55 ctermfg=1

hi NeomakeStatusGood gui=none guibg=#85a400 guifg=#302420 cterm=none ctermbg=2 ctermfg=0
hi NeomakeStatusGoodNC gui=none guibg=#36302a guifg=#85a400 cterm=none ctermbg=8 ctermfg=2

hi NeomakeStatColorDefault gui=none guibg=#799aca guifg=#302420 cterm=none ctermbg=4 ctermfg=0
hi NeomakeStatColorTypeE gui=none guibg=#f46f55 guifg=#302420 cterm=none ctermbg=1 ctermfg=0
hi NeomakeStatColorTypeW gui=none guibg=#b09640 guifg=#302420 cterm=none ctermbg=3 ctermfg=0

" Markdown
" --------
hi MarkdownRule gui=bold guibg=#36302a guifg=#43aa7a cterm=bold ctermbg=8 ctermfg=10

hi MarkdownHeading gui=bold guifg=#a9a2a6 cterm=bold ctermfg=15
hi default link MarkdownH1 MarkdownHeading
hi default link MarkdownH2 MarkdownHeading
hi default link MarkdownH3 MarkdownHeading
hi default link MarkdownH4 MarkdownHeading
hi default link MarkdownH5 MarkdownHeading
hi default link MarkdownH6 MarkdownHeading
hi default link MarkdownHeadingDelimiter MarkdownHeading
hi default link MarkdownHeadingRule MarkdownHeading

hi MarkdownBold gui=bold guifg=#e27e3d cterm=bold ctermfg=9
hi default link MarkdownBoldDelimiter MarkdownBold

hi MarkdownItalic gui=italic guifg=#b09640 cterm=none ctermfg=3
hi default link MarkdownItalicDelimiter MarkdownItalic

hi MarkdownUrl gui=underline guifg=#799aca cterm=underline ctermfg=4
hi MarkdownLinkText gui=none guifg=#958fdf cterm=none ctermfg=12
hi MarkdownLinkDelimiter gui=bold guifg=#a9a2a6 cterm=bold ctermfg=15
hi default link MarkdownLinkTextDelimiter MarkdownLinkDelimiter

hi MarkdownCode guifg=#df798e ctermfg=5
hi default link MarkdownCodeDelimiter MarkdownCode

hi MarkdownCodeBlock guifg=#a9a2a6 ctermfg=15

hi MarkdownListMarker gui=none guifg=#85a400 cterm=none ctermfg=2
hi default link MarkdownOrderedListMarker MarkdownListMarker

" Linting
" -------
hi YcmErrorSection gui=undercurl guisp=#f46f55 cterm=underline
hi YcmWarningSection gui=undercurl guisp=#b09640 cterm=underline
hi SyntasticError gui=undercurl guisp=#f46f55 cterm=underline
hi SyntasticWarning gui=undercurl guisp=#b09640 cterm=underline
hi SyntasticErrorSing guifg=#302420 guibg=#f46f55 ctermfg=0 ctermbg=1
hi SyntasticWarningSign guifg=#302420 guibg=#b09640 ctermfg=0 ctermbg=3
