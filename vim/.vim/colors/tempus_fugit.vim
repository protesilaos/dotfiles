" Name: Tempus Fugit
" Description: Light, pleasant theme optimised for long writing/coding sessions (WCAG AA compliant)
" Author: Protesilaos Stavrou (https://protesilaos.com)
" Meta: Created with the Tempus Themes Generator
" URL: https://gitlab.com/protesilaos/tempus-themes-generator

set background=light
hi clear
if exists("syntax_on")
	syntax reset
endif
let g:colors_name = "tempus_fugit"

" General
" -------
if exists("g:tempus_enforce_background_color")
	hi Normal guibg=#fff5f3 guifg=#4d595f ctermbg=15 ctermfg=0
else
	" NOTE the ctermbg=none is for terminals with transparency
	hi Normal guibg=#fff5f3 guifg=#4d595f ctermbg=none ctermfg=0
endif

hi Visual guibg=#4d595f guifg=#fff5f3 ctermbg=0 ctermfg=15
hi Search gui=underline,bold guibg=#efe6e4 guifg=#4d595f cterm=underline,bold ctermbg=7 ctermfg=3
hi IncSearch gui=underline,bold guibg=#796271 guifg=#fff5f3 term=none cterm=underline,bold ctermbg=8 ctermfg=15

hi StatusLine gui=none guibg=#4d595f guifg=#fff5f3 cterm=none ctermbg=0 ctermfg=15
hi StatusLineNC gui=none guibg=#efe6e4 guifg=#796271 cterm=none ctermbg=7 ctermfg=8
hi StatusLineTerm gui=none guibg=#357200 guifg=#fff5f3 cterm=none ctermbg=2 ctermfg=15
hi StatusLineTermNC gui=none guibg=#efe6e4 guifg=#357200 cterm=none ctermbg=7 ctermfg=2

hi VertSplit gui=none cterm=none
hi TabLine gui=none guibg=#efe6e4 guifg=#796271 cterm=none ctermbg=7 ctermfg=8
hi TabLineSel gui=none guibg=#007072 guifg=#fff5f3 cterm=none ctermbg=6 ctermfg=15
hi TabLineFill gui=none cterm=none

hi Comment gui=italic guifg=#796271 cterm=none ctermfg=8
hi Todo gui=bold guibg=#efe6e4 guifg=#985900 cterm=bold ctermbg=7 ctermfg=11

hi Warning gui=none guibg=#825e00 guifg=#fff5f3 cterm=none ctermbg=3 ctermfg=15
hi WarningMsg gui=none guibg=#825e00 guifg=#fff5f3 cterm=none ctermbg=3 ctermfg=15
hi Error gui=none guibg=#c61a14 guifg=#fff5f3 cterm=none ctermbg=1 ctermfg=15
hi ErrorMsg gui=none guibg=#c61a14 guifg=#fff5f3 cterm=none ctermbg=1 ctermfg=15

hi MatchParen gui=underline,bold guibg=#efe6e4 guifg=#796271 cterm=underline,bold ctermbg=7 ctermfg=8

hi ToolbarLine guibg=#796271 guifg=#fff5f3 term=none ctermbg=8 ctermfg=15
hi ToolbarButton gui=bold guibg=#796271 guifg=#fff5f3 term=none cterm=bold ctermbg=8 ctermfg=15

hi WildMenu guibg=#fff5f3 guifg=#4d595f term=standout ctermbg=15 ctermfg=0

hi Terminal guibg=#fff5f3 guifg=#4d595f term=none ctermbg=15 ctermfg=0

" Constructs
" ----------
hi Constant guifg=#1666b0 ctermfg=4
hi Number guifg=#1666b0 ctermfg=4
hi Float guifg=#1666b0 ctermfg=4
hi String guifg=#485adf ctermfg=12

hi Function guifg=#a83884 ctermfg=5
hi Identifier guifg=#a234c0 term=none ctermfg=13
hi Label guifg=#a83884 ctermfg=5
hi Tag guifg=#a83884 ctermfg=5
hi Keyword gui=bold guifg=#a234c0 gui=bold ctermfg=13

hi Character gui=bold guifg=#00756a cterm=bold ctermfg=14

hi Type gui=none,bold guifg=#007072 term=none cterm=none,bold ctermfg=6
hi Boolean guifg=#007072 ctermfg=6
hi StorageClass guifg=#007072 ctermfg=6
hi Structure guifg=#007072 ctermfg=6
hi Typedef gui=bold guifg=#00756a cterm=bold ctermfg=14

hi Conditional gui=bold guifg=#357200 cterm=bold ctermfg=2
hi Statement gui=none guifg=#437520 cterm=none ctermfg=10
hi Repeat gui=bold guifg=#437520 cterm=bold ctermfg=10
hi Operator gui=bold guifg=#4d595f cterm=bold ctermfg=0
hi Exception gui=bold guifg=#c61a14 cterm=bold ctermfg=1

hi Preproc gui=none guifg=#b93f1a term=none cterm=none ctermfg=9
hi PreCondit gui=bold guifg=#b93f1a cterm=bold ctermfg=9
hi Macro gui=bold guifg=#b93f1a cterm=bold ctermfg=9
hi Include guifg=#b93f1a ctermfg=9
hi Define guifg=#b93f1a ctermfg=9

hi Title gui=bold guibg=#fff5f3 guifg=#007072 cterm=bold ctermbg=15 ctermfg=6

hi Delimeter gui=bold guifg=#a83884 cterm=bold ctermfg=5
hi Delimiter gui=bold guifg=#a83884 cterm=bold ctermfg=5
hi SpecialComment gui=bold guifg=#a83884 cterm=bold ctermfg=5

hi Debug guifg=#a234c0 ctermfg=13

" Other
" -----
hi LineNr guibg=#efe6e4 guifg=#796271 term=none ctermbg=7 ctermfg=8
hi Cursor guibg=#4d595f guifg=#fff5f3 ctermbg=0 ctermfg=15
hi CursorLine gui=none guibg=#efe6e4 term=none cterm=none ctermbg=7
hi CursorColumn gui=none guibg=#efe6e4 term=none cterm=none ctermbg=7
hi CursorLineNr gui=bold guibg=#796271 guifg=#fff5f3 cterm=bold ctermbg=8 ctermfg=15
hi ColorColumn guibg=#efe6e4 guifg=#4d595f term=none ctermbg=7 ctermfg=0
hi SignColumn guibg=#efe6e4 guifg=#796271 term=none ctermbg=7 ctermfg=8

hi Folded guibg=#efe6e4 guifg=#796271 ctermbg=7 ctermfg=8
hi FoldColumn guibg=#efe6e4 guifg=#796271 ctermbg=7 ctermfg=8

hi Special gui=bold guifg=#985900 term=none cterm=bold ctermfg=11
hi SpecialKey gui=none guibg=#efe6e4 guifg=#796271 cterm=none ctermbg=7 ctermfg=8
hi SpecialChar gui=bold guifg=#985900 cterm=bold ctermfg=11
hi NonText gui=none guibg=#efe6e4 guifg=#796271 cterm=none ctermbg=7 ctermfg=8
hi EndOfBuffer gui=bold guifg=#796271 cterm=bold ctermfg=8

hi Directory gui=none guifg=#357200 term=none cterm=none ctermfg=2
hi Question gui=bold guifg=#985900 cterm=bold ctermfg=11
hi MoreMsg guifg=#437520 ctermfg=10
hi ModeMsg gui=bold guifg=#357200 cterm=bold ctermfg=2

hi VimOption guifg=#a83884 ctermfg=5
hi VimGroup guifg=#a83884 ctermfg=5

hi Underlined gui=underline guifg=#4d595f cterm=underline ctermfg=0
hi Ignore guibg=#efe6e4 guifg=#796271 ctermbg=7 ctermfg=8
hi Conceal guibg=#796271 guifg=#efe6e4 ctermbg=8 ctermfg=7

hi SpellBad guibg=#c61a14 guifg=#fff5f3 ctermbg=1 ctermfg=15
hi SpellCap guibg=#825e00 guifg=#fff5f3 ctermbg=3 ctermfg=15
hi SpellRare guibg=#a234c0 guifg=#fff5f3 ctermbg=13 ctermfg=15
hi SpellLocal guibg=#00756a guifg=#fff5f3 ctermbg=14 ctermfg=15

hi Pmenu gui=italic guibg=#efe6e4 guifg=#4d595f cterm=none ctermbg=7 ctermfg=0
hi PmenuSel gui=none,bold guibg=#796271 guifg=#fff5f3 cterm=none,bold ctermbg=8 ctermfg=15
hi PmenuSbar guibg=#efe6e4 ctermbg=7
hi PmenuThumb guibg=#796271 ctermbg=8

" Terminal
" --------

if exists('*term_setansicolors')
	let g:terminal_ansi_colors = [
				\ "#4d595f",
				\ "#c61a14",
				\ "#357200",
				\ "#825e00",
				\ "#1666b0",
				\ "#a83884",
				\ "#007072",
				\ "#efe6e4",
				\ "#796271",
				\ "#b93f1a",
				\ "#437520",
				\ "#985900",
				\ "#485adf",
				\ "#a234c0",
				\ "#00756a",
				\ "#fff5f3"
				\ ]
endif
if has('nvim')
	let g:terminal_color_0 = "#4d595f"
	let g:terminal_color_1 = "#c61a14"
	let g:terminal_color_2 = "#357200"
	let g:terminal_color_3 = "#825e00"
	let g:terminal_color_4 = "#1666b0"
	let g:terminal_color_5 = "#a83884"
	let g:terminal_color_6 = "#007072"
	let g:terminal_color_7 = "#efe6e4"
	let g:terminal_color_8 = "#796271"
	let g:terminal_color_9 = "#b93f1a"
	let g:terminal_color_10 = "#437520"
	let g:terminal_color_11 = "#985900"
	let g:terminal_color_12 = "#485adf"
	let g:terminal_color_13 = "#a234c0"
	let g:terminal_color_14 = "#00756a"
	let g:terminal_color_15 = "#fff5f3"
endif

" Diffs
" -----
hi DiffAdd gui=bold guibg=#357200 guifg=#fff5f3 cterm=bold ctermbg=10 ctermfg=15
hi DiffDelete gui=none guibg=#c61a14 guifg=#fff5f3 cterm=none ctermbg=9 ctermfg=15
hi DiffChange gui=bold guibg=#efe6e4 guifg=#796271 cterm=bold ctermbg=7 ctermfg=8
hi DiffText gui=bold guibg=#efe6e4 guifg=#b93f1a cterm=bold ctermbg=7 ctermfg=1

hi diffAdded guifg=#357200 ctermfg=2
hi diffRemoved guifg=#c61a14 ctermfg=1
hi diffNewFile gui=none guifg=#1666b0 ctermfg=4
hi diffFile gui=none guifg=#825e00 cterm=none ctermfg=3

hi GitGutterAdd guibg=#efe6e4 guifg=#357200 ctermbg=7 ctermfg=2
hi GitGutterChange gui=bold guibg=#efe6e4 guifg=#796271 cterm=bold ctermbg=7 ctermfg=8
hi GitGutterDelete guibg=#efe6e4 guifg=#c61a14 ctermbg=7 ctermfg=1
hi GitGutterChangeDelete gui=bold guibg=#efe6e4 guifg=#c61a14 cterm=bold ctermbg=7 ctermfg=1

" Neomake
" -------
hi NeomakeError gui=none guibg=#c61a14 guifg=#fff5f3 cterm=none ctermbg=1 ctermfg=15
hi NeomakeInfo gui=none guibg=#007072 guifg=#fff5f3 cterm=none ctermbg=6 ctermfg=15
hi NeomakeWarning gui=none guibg=#825e00 guifg=#fff5f3 cterm=none ctermbg=3 ctermfg=15
hi NeomakeMessage gui=none guibg=#985900 guifg=#fff5f3 cterm=none ctermbg=11 ctermfg=15

hi NeomakeVirtualtextInfoDefault guifg=#00756a ctermfg=14
hi NeomakeVirtualtextMessageDefault guifg=#985900 ctermfg=11
hi NeomakeVirtualtextWarningDefault guifg=#825e00 ctermfg=3
hi NeomakeVirtualtextErrorDefault guifg=#c61a14 ctermfg=1

hi NeomakeStatusGood gui=none guibg=#357200 guifg=#fff5f3 cterm=none ctermbg=2 ctermfg=15
hi NeomakeStatusGoodNC gui=none guibg=#efe6e4 guifg=#357200 cterm=none ctermbg=7 ctermfg=2

hi NeomakeStatColorDefault gui=none guibg=#1666b0 guifg=#fff5f3 cterm=none ctermbg=4 ctermfg=15
hi NeomakeStatColorTypeE gui=none guibg=#c61a14 guifg=#fff5f3 cterm=none ctermbg=1 ctermfg=15
hi NeomakeStatColorTypeW gui=none guibg=#825e00 guifg=#fff5f3 cterm=none ctermbg=3 ctermfg=15

" Markdown
" --------
hi MarkdownRule gui=bold guibg=#efe6e4 guifg=#437520 cterm=bold ctermbg=7 ctermfg=10

hi MarkdownHeading gui=bold guifg=#4d595f cterm=bold ctermfg=0
hi default link MarkdownH1 MarkdownHeading
hi default link MarkdownH2 MarkdownHeading
hi default link MarkdownH3 MarkdownHeading
hi default link MarkdownH4 MarkdownHeading
hi default link MarkdownH5 MarkdownHeading
hi default link MarkdownH6 MarkdownHeading
hi default link MarkdownHeadingDelimiter MarkdownHeading
hi default link MarkdownHeadingRule MarkdownHeading

hi MarkdownBold gui=bold guifg=#b93f1a cterm=bold ctermfg=9
hi default link MarkdownBoldDelimiter MarkdownBold

hi MarkdownItalic gui=italic guifg=#825e00 cterm=none ctermfg=3
hi default link MarkdownItalicDelimiter MarkdownItalic

hi MarkdownUrl gui=underline guifg=#1666b0 cterm=underline ctermfg=4
hi MarkdownLinkText gui=none guifg=#485adf cterm=none ctermfg=12
hi MarkdownLinkDelimiter gui=bold guifg=#4d595f cterm=bold ctermfg=0
hi default link MarkdownLinkTextDelimiter MarkdownLinkDelimiter

hi MarkdownCode guifg=#a83884 ctermfg=5
hi default link MarkdownCodeDelimiter MarkdownCode

hi MarkdownCodeBlock guifg=#4d595f ctermfg=0

hi MarkdownListMarker gui=none guifg=#357200 cterm=none ctermfg=2
hi default link MarkdownOrderedListMarker MarkdownListMarker

" Linting
" -------
hi YcmErrorSection gui=undercurl guisp=#c61a14 cterm=underline
hi YcmWarningSection gui=undercurl guisp=#1666b0 cterm=underline
hi SyntasticError gui=undercurl guisp=#c61a14 cterm=underline
hi SyntasticWarning gui=undercurl guisp=#1666b0 cterm=underline
hi SyntasticErrorSing guifg=#fff5f3 guibg=#c61a14 ctermfg=15 ctermbg=1
hi SyntasticWarningSign guifg=#fff5f3 guibg=#1666b0 ctermfg=15 ctermbg=4
