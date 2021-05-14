" Name: Tempus Classic
" Description: Dark theme with warm hues (WCAG AA compliant)
" Author: Protesilaos Stavrou (https://protesilaos.com)
" Meta: Created with the Tempus Themes Generator
" URL: https://gitlab.com/protesilaos/tempus-themes-generator

set background=dark
hi clear
if exists("syntax_on")
	syntax reset
endif
let g:colors_name = "tempus_classic"

" General
" -------
if exists("g:tempus_enforce_background_color")
	hi Normal guibg=#232323 guifg=#aeadaf ctermbg=0 ctermfg=15
else
	" NOTE the ctermbg=none is for terminals with transparency
	hi Normal guibg=#232323 guifg=#aeadaf ctermbg=none ctermfg=15
endif

hi Visual guibg=#aeadaf guifg=#232323 ctermbg=15 ctermfg=0
hi Search gui=underline,bold guibg=#312e30 guifg=#aeadaf cterm=underline,bold ctermbg=8 ctermfg=3
hi IncSearch gui=underline,bold guibg=#949d9f guifg=#232323 term=none cterm=underline,bold ctermbg=7 ctermfg=0

hi StatusLine gui=none guibg=#aeadaf guifg=#232323 cterm=none ctermbg=15 ctermfg=0
hi StatusLineNC gui=none guibg=#312e30 guifg=#949d9f cterm=none ctermbg=8 ctermfg=7
hi StatusLineTerm gui=none guibg=#8c9e3d guifg=#232323 cterm=none ctermbg=2 ctermfg=0
hi StatusLineTermNC gui=none guibg=#312e30 guifg=#8c9e3d cterm=none ctermbg=8 ctermfg=2

hi VertSplit gui=none cterm=none
hi TabLine gui=none guibg=#312e30 guifg=#949d9f cterm=none ctermbg=8 ctermfg=7
hi TabLineSel gui=none guibg=#6da280 guifg=#232323 cterm=none ctermbg=6 ctermfg=0
hi TabLineFill gui=none cterm=none

hi Comment gui=italic guifg=#949d9f cterm=none ctermfg=7
hi Todo gui=bold guibg=#312e30 guifg=#a8a030 cterm=bold ctermbg=8 ctermfg=11

hi Warning gui=none guibg=#b1942b guifg=#232323 cterm=none ctermbg=3 ctermfg=0
hi WarningMsg gui=none guibg=#b1942b guifg=#232323 cterm=none ctermbg=3 ctermfg=0
hi Error gui=none guibg=#d4823d guifg=#232323 cterm=none ctermbg=1 ctermfg=0
hi ErrorMsg gui=none guibg=#d4823d guifg=#232323 cterm=none ctermbg=1 ctermfg=0

hi MatchParen gui=underline,bold guibg=#312e30 guifg=#949d9f cterm=underline,bold ctermbg=8 ctermfg=7

hi ToolbarLine guibg=#949d9f guifg=#232323 term=none ctermbg=7 ctermfg=0
hi ToolbarButton gui=bold guibg=#949d9f guifg=#232323 term=none cterm=bold ctermbg=7 ctermfg=0

hi WildMenu guibg=#232323 guifg=#aeadaf term=standout ctermbg=0 ctermfg=15

hi Terminal guibg=#232323 guifg=#aeadaf term=none ctermbg=0 ctermfg=15

" Constructs
" ----------
hi Constant guifg=#6e9cb0 ctermfg=4
hi Number guifg=#6e9cb0 ctermfg=4
hi Float guifg=#6e9cb0 ctermfg=4
hi String guifg=#8e9cc0 ctermfg=12

hi Function guifg=#b58d88 ctermfg=5
hi Identifier guifg=#d58888 term=none ctermfg=13
hi Label guifg=#b58d88 ctermfg=5
hi Tag guifg=#b58d88 ctermfg=5
hi Keyword gui=bold guifg=#d58888 gui=bold ctermfg=13

hi Character gui=bold guifg=#7aa880 cterm=bold ctermfg=14

hi Type gui=none,bold guifg=#6da280 term=none cterm=none,bold ctermfg=6
hi Boolean guifg=#6da280 ctermfg=6
hi StorageClass guifg=#6da280 ctermfg=6
hi Structure guifg=#6da280 ctermfg=6
hi Typedef gui=bold guifg=#7aa880 cterm=bold ctermfg=14

hi Conditional gui=bold guifg=#8c9e3d cterm=bold ctermfg=2
hi Statement gui=none guifg=#96a42d cterm=none ctermfg=10
hi Repeat gui=bold guifg=#96a42d cterm=bold ctermfg=10
hi Operator gui=bold guifg=#aeadaf cterm=bold ctermfg=15
hi Exception gui=bold guifg=#d4823d cterm=bold ctermfg=1

hi Preproc gui=none guifg=#d0913d term=none cterm=none ctermfg=9
hi PreCondit gui=bold guifg=#d0913d cterm=bold ctermfg=9
hi Macro gui=bold guifg=#d0913d cterm=bold ctermfg=9
hi Include guifg=#d0913d ctermfg=9
hi Define guifg=#d0913d ctermfg=9

hi Title gui=bold guibg=#232323 guifg=#6da280 cterm=bold ctermbg=0 ctermfg=6

hi Delimeter gui=bold guifg=#b58d88 cterm=bold ctermfg=5
hi Delimiter gui=bold guifg=#b58d88 cterm=bold ctermfg=5
hi SpecialComment gui=bold guifg=#b58d88 cterm=bold ctermfg=5

hi Debug guifg=#d58888 ctermfg=13

" Other
" -----
hi LineNr guibg=#312e30 guifg=#949d9f term=none ctermbg=8 ctermfg=7
hi Cursor guibg=#aeadaf guifg=#232323 ctermbg=15 ctermfg=0
hi CursorLine gui=none guibg=#312e30 term=none cterm=none ctermbg=8
hi CursorColumn gui=none guibg=#312e30 term=none cterm=none ctermbg=8
hi CursorLineNr gui=bold guibg=#949d9f guifg=#232323 cterm=bold ctermbg=7 ctermfg=0
hi ColorColumn guibg=#312e30 guifg=#aeadaf term=none ctermbg=8 ctermfg=15
hi SignColumn guibg=#312e30 guifg=#949d9f term=none ctermbg=8 ctermfg=7

hi Folded guibg=#312e30 guifg=#949d9f ctermbg=8 ctermfg=7
hi FoldColumn guibg=#312e30 guifg=#949d9f ctermbg=8 ctermfg=7

hi Special gui=bold guifg=#a8a030 term=none cterm=bold ctermfg=11
hi SpecialKey gui=none guibg=#312e30 guifg=#949d9f cterm=none ctermbg=8 ctermfg=7
hi SpecialChar gui=bold guifg=#a8a030 cterm=bold ctermfg=11
hi NonText gui=none guibg=#312e30 guifg=#949d9f cterm=none ctermbg=8 ctermfg=7
hi EndOfBuffer gui=bold guifg=#949d9f cterm=bold ctermfg=7

hi Directory gui=none guifg=#8c9e3d term=none cterm=none ctermfg=2
hi Question gui=bold guifg=#a8a030 cterm=bold ctermfg=11
hi MoreMsg guifg=#96a42d ctermfg=10
hi ModeMsg gui=bold guifg=#8c9e3d cterm=bold ctermfg=2

hi VimOption guifg=#b58d88 ctermfg=5
hi VimGroup guifg=#b58d88 ctermfg=5

hi Underlined gui=underline guifg=#aeadaf cterm=underline ctermfg=15
hi Ignore guibg=#312e30 guifg=#949d9f ctermbg=8 ctermfg=7
hi Conceal guibg=#949d9f guifg=#312e30 ctermbg=7 ctermfg=8

hi SpellBad guibg=#d4823d guifg=#232323 ctermbg=1 ctermfg=0
hi SpellCap guibg=#b1942b guifg=#232323 ctermbg=3 ctermfg=0
hi SpellRare guibg=#d58888 guifg=#232323 ctermbg=13 ctermfg=0
hi SpellLocal guibg=#7aa880 guifg=#232323 ctermbg=14 ctermfg=0

hi Pmenu gui=italic guibg=#312e30 guifg=#aeadaf cterm=none ctermbg=8 ctermfg=15
hi PmenuSel gui=none,bold guibg=#949d9f guifg=#232323 cterm=none,bold ctermbg=7 ctermfg=0
hi PmenuSbar guibg=#312e30 ctermbg=8
hi PmenuThumb guibg=#949d9f ctermbg=7

" Terminal
" --------

if exists('*term_setansicolors')
	let g:terminal_ansi_colors = [
				\ "#232323",
				\ "#d4823d",
				\ "#8c9e3d",
				\ "#b1942b",
				\ "#6e9cb0",
				\ "#b58d88",
				\ "#6da280",
				\ "#949d9f",
				\ "#312e30",
				\ "#d0913d",
				\ "#96a42d",
				\ "#a8a030",
				\ "#8e9cc0",
				\ "#d58888",
				\ "#7aa880",
				\ "#aeadaf"
				\ ]
endif
if has('nvim')
	let g:terminal_color_0 = "#232323"
	let g:terminal_color_1 = "#d4823d"
	let g:terminal_color_2 = "#8c9e3d"
	let g:terminal_color_3 = "#b1942b"
	let g:terminal_color_4 = "#6e9cb0"
	let g:terminal_color_5 = "#b58d88"
	let g:terminal_color_6 = "#6da280"
	let g:terminal_color_7 = "#949d9f"
	let g:terminal_color_8 = "#312e30"
	let g:terminal_color_9 = "#d0913d"
	let g:terminal_color_10 = "#96a42d"
	let g:terminal_color_11 = "#a8a030"
	let g:terminal_color_12 = "#8e9cc0"
	let g:terminal_color_13 = "#d58888"
	let g:terminal_color_14 = "#7aa880"
	let g:terminal_color_15 = "#aeadaf"
endif

" Diffs
" -----
hi DiffAdd gui=bold guibg=#8c9e3d guifg=#232323 cterm=bold ctermbg=10 ctermfg=0
hi DiffDelete gui=none guibg=#d4823d guifg=#232323 cterm=none ctermbg=9 ctermfg=0
hi DiffChange gui=bold guibg=#312e30 guifg=#949d9f cterm=bold ctermbg=8 ctermfg=7
hi DiffText gui=bold guibg=#312e30 guifg=#d0913d cterm=bold ctermbg=8 ctermfg=1

hi diffAdded guifg=#8c9e3d ctermfg=2
hi diffRemoved guifg=#d4823d ctermfg=1
hi diffNewFile gui=none guifg=#6e9cb0 ctermfg=4
hi diffFile gui=none guifg=#b1942b cterm=none ctermfg=3

hi GitGutterAdd guibg=#312e30 guifg=#8c9e3d ctermbg=8 ctermfg=2
hi GitGutterChange gui=bold guibg=#312e30 guifg=#949d9f cterm=bold ctermbg=8 ctermfg=7
hi GitGutterDelete guibg=#312e30 guifg=#d4823d ctermbg=8 ctermfg=1
hi GitGutterChangeDelete gui=bold guibg=#312e30 guifg=#d4823d cterm=bold ctermbg=8 ctermfg=1

" Neomake
" -------
hi NeomakeError gui=none guibg=#d4823d guifg=#232323 cterm=none ctermbg=1 ctermfg=0
hi NeomakeInfo gui=none guibg=#6da280 guifg=#232323 cterm=none ctermbg=6 ctermfg=0
hi NeomakeWarning gui=none guibg=#b1942b guifg=#232323 cterm=none ctermbg=3 ctermfg=0
hi NeomakeMessage gui=none guibg=#a8a030 guifg=#232323 cterm=none ctermbg=11 ctermfg=0

hi NeomakeVirtualtextInfoDefault guifg=#7aa880 ctermfg=14
hi NeomakeVirtualtextMessageDefault guifg=#a8a030 ctermfg=11
hi NeomakeVirtualtextWarningDefault guifg=#b1942b ctermfg=3
hi NeomakeVirtualtextErrorDefault guifg=#d4823d ctermfg=1

hi NeomakeStatusGood gui=none guibg=#8c9e3d guifg=#232323 cterm=none ctermbg=2 ctermfg=0
hi NeomakeStatusGoodNC gui=none guibg=#312e30 guifg=#8c9e3d cterm=none ctermbg=8 ctermfg=2

hi NeomakeStatColorDefault gui=none guibg=#6e9cb0 guifg=#232323 cterm=none ctermbg=4 ctermfg=0
hi NeomakeStatColorTypeE gui=none guibg=#d4823d guifg=#232323 cterm=none ctermbg=1 ctermfg=0
hi NeomakeStatColorTypeW gui=none guibg=#b1942b guifg=#232323 cterm=none ctermbg=3 ctermfg=0

" Markdown
" --------
hi MarkdownRule gui=bold guibg=#312e30 guifg=#96a42d cterm=bold ctermbg=8 ctermfg=10

hi MarkdownHeading gui=bold guifg=#aeadaf cterm=bold ctermfg=15
hi default link MarkdownH1 MarkdownHeading
hi default link MarkdownH2 MarkdownHeading
hi default link MarkdownH3 MarkdownHeading
hi default link MarkdownH4 MarkdownHeading
hi default link MarkdownH5 MarkdownHeading
hi default link MarkdownH6 MarkdownHeading
hi default link MarkdownHeadingDelimiter MarkdownHeading
hi default link MarkdownHeadingRule MarkdownHeading

hi MarkdownBold gui=bold guifg=#d0913d cterm=bold ctermfg=9
hi default link MarkdownBoldDelimiter MarkdownBold

hi MarkdownItalic gui=italic guifg=#b1942b cterm=none ctermfg=3
hi default link MarkdownItalicDelimiter MarkdownItalic

hi MarkdownUrl gui=underline guifg=#6e9cb0 cterm=underline ctermfg=4
hi MarkdownLinkText gui=none guifg=#8e9cc0 cterm=none ctermfg=12
hi MarkdownLinkDelimiter gui=bold guifg=#aeadaf cterm=bold ctermfg=15
hi default link MarkdownLinkTextDelimiter MarkdownLinkDelimiter

hi MarkdownCode guifg=#b58d88 ctermfg=5
hi default link MarkdownCodeDelimiter MarkdownCode

hi MarkdownCodeBlock guifg=#aeadaf ctermfg=15

hi MarkdownListMarker gui=none guifg=#8c9e3d cterm=none ctermfg=2
hi default link MarkdownOrderedListMarker MarkdownListMarker

" Linting
" -------
hi YcmErrorSection gui=undercurl guisp=#d4823d cterm=underline
hi YcmWarningSection gui=undercurl guisp=#b1942b cterm=underline
hi SyntasticError gui=undercurl guisp=#d4823d cterm=underline
hi SyntasticWarning gui=undercurl guisp=#b1942b cterm=underline
hi SyntasticErrorSing guifg=#232323 guibg=#d4823d ctermfg=0 ctermbg=1
hi SyntasticWarningSign guifg=#232323 guibg=#b1942b ctermfg=0 ctermbg=3
