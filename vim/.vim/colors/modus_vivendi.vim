" vi: ft=vim
" Name: modus_vivendi
" Description: Vim port of modus-vivendi (Modus themes for Emacs)
" Author: Protesilaos Stavrou, <https://protesilaos.com>
" Meta: Created with the modus-themes-exporter.el library
" URL: https://git.sr.ht/~protesilaos/modus-themes

set background=dark
hi clear
if exists('syntax_on')
	syntax reset
endif
let g:colors_name = 'modus_vivendi'

" General
" -------
if exists('g:modus_themes_enforce_background_color')
	hi Normal guibg=#000000 guifg=#ffffff ctermbg=0 ctermfg=15
else
	" NOTE the ctermbg=none is for terminals with transparency
	hi Normal guibg=#000000 guifg=#ffffff ctermbg=none ctermfg=15
endif

hi Visual guibg=#ffffff guifg=#000000 ctermbg=15 ctermfg=0
hi Search gui=underline,bold guibg=#191a1b guifg=#ffffff cterm=underline,bold ctermbg=8 ctermfg=3
hi IncSearch gui=underline,bold guibg=#a8a8a8 guifg=#000000 term=none cterm=underline,bold ctermbg=7 ctermfg=0

hi StatusLine gui=none guibg=#ffffff guifg=#000000 cterm=none ctermbg=15 ctermfg=0
hi StatusLineNC gui=none guibg=#191a1b guifg=#a8a8a8 cterm=none ctermbg=8 ctermfg=7
hi StatusLineTerm gui=none guibg=#44bc44 guifg=#000000 cterm=none ctermbg=2 ctermfg=0
hi StatusLineTermNC gui=none guibg=#191a1b guifg=#44bc44 cterm=none ctermbg=8 ctermfg=2

hi VertSplit gui=none cterm=none
hi TabLine gui=none guibg=#191a1b guifg=#a8a8a8 cterm=none ctermbg=8 ctermfg=7
hi TabLineSel gui=none guibg=#00d3d0 guifg=#000000 cterm=none ctermbg=6 ctermfg=0
hi TabLineFill gui=none cterm=none

hi Comment gui=italic guifg=#a8a8a8 cterm=none ctermfg=7
hi Todo guibg=#100f10 guifg=#cfdf30 ctermbg=0 ctermfg=11

hi Warning gui=none guibg=#eecc00 guifg=#000000 cterm=none ctermbg=3 ctermfg=0
hi WarningMsg gui=none guibg=#eecc00 guifg=#000000 cterm=none ctermbg=3 ctermfg=0
hi Error gui=none guibg=#ff8059 guifg=#000000 cterm=none ctermbg=1 ctermfg=0
hi ErrorMsg gui=none guibg=#ff8059 guifg=#000000 cterm=none ctermbg=1 ctermfg=0

hi MatchParen gui=underline,bold guibg=#191a1b guifg=#a8a8a8 cterm=underline,bold ctermbg=8 ctermfg=7

hi ToolbarLine guibg=#a8a8a8 guifg=#000000 term=none ctermbg=7 ctermfg=0
hi ToolbarButton gui=bold guibg=#a8a8a8 guifg=#000000 term=none cterm=bold ctermbg=7 ctermfg=0

hi WildMenu guibg=#000000 guifg=#ffffff term=standout ctermbg=0 ctermfg=15

hi Terminal guibg=#000000 guifg=#ffffff term=none ctermbg=0 ctermfg=15

" Constructs
" ----------
hi Constant guifg=#00bcff ctermfg=4
hi Boolean guifg=#44bc44 ctermfg=2
hi Number guifg=#00bcff ctermfg=4
hi Float guifg=#00bcff ctermfg=4
hi String guifg=#79a8ff ctermfg=12

hi Function guifg=#feacd0 ctermfg=5
hi Identifier gui=none guifg=#00d3d0 cterm=none ctermfg=6
hi Label guifg=#feacd0 ctermfg=5
hi Tag guifg=#feacd0 ctermfg=5
hi Keyword gui=none guifg=#f78fe7 cterm=none ctermfg=5

hi Character gui=bold guifg=#6ae4b9 cterm=bold ctermfg=14

hi Type gui=none,bold guifg=#00d3d0 term=none cterm=none,bold ctermfg=6
hi StorageClass guifg=#00d3d0 ctermfg=6
hi Structure guifg=#00d3d0 ctermfg=6
hi Typedef gui=bold guifg=#6ae4b9 cterm=bold ctermfg=14

hi Conditional gui=none guifg=#b6a0ff cterm=none ctermfg=13
hi Statement gui=none guifg=#b6a0ff cterm=none ctermfg=13
hi Repeat gui=bold guifg=#f78fe7 cterm=bold ctermfg=5
hi Operator gui=none guifg=#ffffff cterm=none ctermfg=15
hi Exception gui=bold guifg=#eecc00 cterm=bold ctermfg=2

hi Preproc gui=none guifg=#f4923b term=none cterm=none ctermfg=9
hi PreCondit gui=bold guifg=#f4923b cterm=bold ctermfg=9
hi Macro gui=bold guifg=#f78fe7 cterm=bold ctermfg=5
hi Include guifg=#f4923b ctermfg=9
hi Define guifg=#f4923b ctermfg=9

hi Title gui=bold guibg=#000000 guifg=#00d3d0 cterm=bold ctermbg=0 ctermfg=6

hi Delimiter gui=none guifg=#ffffff cterm=none ctermfg=15
hi SpecialComment gui=bold guifg=#f0ce43 cterm=bold ctermfg=3

hi Debug guifg=#f78fe7 ctermfg=13

" Other
" -----
hi LineNr guibg=#191a1b guifg=#a8a8a8 term=none ctermbg=8 ctermfg=7
hi Cursor guibg=#ffffff guifg=#000000 ctermbg=15 ctermfg=0
hi CursorLine gui=none guibg=#191a1b term=none cterm=none ctermbg=8
hi CursorColumn gui=none guibg=#191a1b term=none cterm=none ctermbg=8
hi CursorLineNr gui=bold guibg=#a8a8a8 guifg=#000000 cterm=bold ctermbg=7 ctermfg=0
hi ColorColumn guibg=#191a1b guifg=#ffffff term=none ctermbg=8 ctermfg=15
hi SignColumn guibg=#191a1b guifg=#a8a8a8 term=none ctermbg=8 ctermfg=7

hi Folded guibg=#191a1b guifg=#a8a8a8 ctermbg=8 ctermfg=7
hi FoldColumn guibg=#191a1b guifg=#a8a8a8 ctermbg=8 ctermfg=7

hi Special gui=none guifg=#00cd68 term=none cterm=none ctermfg=2
hi SpecialKey gui=none guibg=#191a1b guifg=#a8a8a8 cterm=none ctermbg=8 ctermfg=7
hi SpecialChar gui=bold guifg=#cfdf30 cterm=bold ctermfg=11
hi NonText gui=none guibg=#191a1b guifg=#a8a8a8 cterm=none ctermbg=8 ctermfg=7
hi EndOfBuffer gui=bold guifg=#a8a8a8 cterm=bold ctermfg=7

hi Directory gui=none guifg=#44bc44 term=none cterm=none ctermfg=2
hi Question gui=bold guifg=#cfdf30 cterm=bold ctermfg=11
hi MoreMsg guifg=#70c900 ctermfg=10
hi ModeMsg gui=bold guifg=#44bc44 cterm=bold ctermfg=2

hi VimOption guifg=#4ae8fc ctermfg=6
hi VimGroup guifg=#4ae8fc ctermfg=5

hi Underlined gui=underline guifg=#ffffff cterm=underline ctermfg=15
hi Ignore guibg=#191a1b guifg=#a8a8a8 ctermbg=8 ctermfg=7
hi Conceal guibg=#a8a8a8 guifg=#191a1b ctermbg=7 ctermfg=8

hi SpellBad guibg=#ff8059 guifg=#000000 ctermbg=1 ctermfg=0
hi SpellCap guibg=#eecc00 guifg=#000000 ctermbg=3 ctermfg=0
hi SpellRare guibg=#b6a0ff guifg=#000000 ctermbg=13 ctermfg=0
hi SpellLocal guibg=#6ae4b9 guifg=#000000 ctermbg=14 ctermfg=0

hi Pmenu gui=italic guibg=#191a1b guifg=#ffffff cterm=none ctermbg=8 ctermfg=15
hi PmenuSel gui=none,bold guibg=#a8a8a8 guifg=#000000 cterm=none,bold ctermbg=7 ctermfg=0
hi PmenuSbar guibg=#191a1b ctermbg=8
hi PmenuThumb guibg=#a8a8a8 ctermbg=7

" Terminal
" --------

if exists('*term_setansicolors')
	let g:terminal_ansi_colors = [
				\ '#000000',
				\ '#ff8059',
				\ '#44bc44',
				\ '#eecc00',
				\ '#2fafff',
				\ '#feacd0',
				\ '#00d3d0',
				\ '#bfbfbf',
				\ '#595959',
				\ '#f4923b',
				\ '#70c900',
				\ '#cfdf30',
				\ '#79a8ff',
				\ '#b6a0ff',
				\ '#6ae4b9',
				\ '#ffffff'
				\ ]
endif
if has('nvim')
	let g:terminal_color_0 = '#000000'
	let g:terminal_color_1 = '#ff8059'
	let g:terminal_color_2 = '#44bc44'
	let g:terminal_color_3 = '#eecc00'
	let g:terminal_color_4 = '#2fafff'
	let g:terminal_color_5 = '#feacd0'
	let g:terminal_color_6 = '#00d3d0'
	let g:terminal_color_7 = '#bfbfbf'
	let g:terminal_color_8 = '#595959'
	let g:terminal_color_9 = '#f4923b'
	let g:terminal_color_10 = '#70c900'
	let g:terminal_color_11 = '#cfdf30'
	let g:terminal_color_12 = '#79a8ff'
	let g:terminal_color_13 = '#b6a0ff'
	let g:terminal_color_14 = '#6ae4b9'
	let g:terminal_color_15 = '#ffffff'
endif

" Diffs
" -----
hi DiffAdd gui=bold guibg=#44bc44 guifg=#000000 cterm=bold ctermbg=10 ctermfg=0
hi DiffDelete gui=none guibg=#ff8059 guifg=#000000 cterm=none ctermbg=9 ctermfg=0
hi DiffChange gui=bold guibg=#191a1b guifg=#a8a8a8 cterm=bold ctermbg=8 ctermfg=7
hi DiffText gui=bold guibg=#191a1b guifg=#f4923b cterm=bold ctermbg=8 ctermfg=1

hi diffAdded guifg=#44bc44 ctermfg=2
hi diffRemoved guifg=#ff8059 ctermfg=1
hi diffNewFile gui=none guifg=#2fafff ctermfg=4
hi diffFile gui=none guifg=#eecc00 cterm=none ctermfg=3

hi GitGutterAdd guibg=#191a1b guifg=#44bc44 ctermbg=8 ctermfg=2
hi GitGutterChange gui=bold guibg=#191a1b guifg=#a8a8a8 cterm=bold ctermbg=8 ctermfg=7
hi GitGutterDelete guibg=#191a1b guifg=#ff8059 ctermbg=8 ctermfg=1
hi GitGutterChangeDelete gui=bold guibg=#191a1b guifg=#ff8059 cterm=bold ctermbg=8 ctermfg=1

" Neomake
" -------
hi NeomakeError gui=none guibg=#ff8059 guifg=#000000 cterm=none ctermbg=1 ctermfg=0
hi NeomakeInfo gui=none guibg=#00d3d0 guifg=#000000 cterm=none ctermbg=6 ctermfg=0
hi NeomakeWarning gui=none guibg=#eecc00 guifg=#000000 cterm=none ctermbg=3 ctermfg=0
hi NeomakeMessage gui=none guibg=#cfdf30 guifg=#000000 cterm=none ctermbg=11 ctermfg=0

hi NeomakeVirtualtextInfoDefault guifg=#6ae4b9 ctermfg=14
hi NeomakeVirtualtextMessageDefault guifg=#cfdf30 ctermfg=11
hi NeomakeVirtualtextWarningDefault guifg=#eecc00 ctermfg=3
hi NeomakeVirtualtextErrorDefault guifg=#ff8059 ctermfg=1

hi NeomakeStatusGood gui=none guibg=#44bc44 guifg=#000000 cterm=none ctermbg=2 ctermfg=0
hi NeomakeStatusGoodNC gui=none guibg=#191a1b guifg=#44bc44 cterm=none ctermbg=8 ctermfg=2

hi NeomakeStatColorDefault gui=none guibg=#2fafff guifg=#000000 cterm=none ctermbg=4 ctermfg=0
hi NeomakeStatColorTypeE gui=none guibg=#ff8059 guifg=#000000 cterm=none ctermbg=1 ctermfg=0
hi NeomakeStatColorTypeW gui=none guibg=#eecc00 guifg=#000000 cterm=none ctermbg=3 ctermfg=0

" Markdown
" --------
hi MarkdownRule gui=bold guibg=#191a1b guifg=#70c900 cterm=bold ctermbg=8 ctermfg=10

hi MarkdownHeading gui=bold guifg=#ffffff cterm=bold ctermfg=15
hi default link MarkdownH1 MarkdownHeading
hi default link MarkdownH2 MarkdownHeading
hi default link MarkdownH3 MarkdownHeading
hi default link MarkdownH4 MarkdownHeading
hi default link MarkdownH5 MarkdownHeading
hi default link MarkdownH6 MarkdownHeading
hi default link MarkdownHeadingDelimiter MarkdownHeading
hi default link MarkdownHeadingRule MarkdownHeading

hi MarkdownBold gui=bold guifg=#f4923b cterm=bold ctermfg=9
hi default link MarkdownBoldDelimiter MarkdownBold

hi MarkdownItalic gui=italic guifg=#eecc00 cterm=none ctermfg=3
hi default link MarkdownItalicDelimiter MarkdownItalic

hi MarkdownUrl gui=underline guifg=#2fafff cterm=underline ctermfg=4
hi MarkdownLinkText gui=none guifg=#79a8ff cterm=none ctermfg=12
hi MarkdownLinkDelimiter gui=bold guifg=#ffffff cterm=bold ctermfg=15
hi default link MarkdownLinkTextDelimiter MarkdownLinkDelimiter

hi MarkdownCode guifg=#feacd0 ctermfg=5
hi default link MarkdownCodeDelimiter MarkdownCode

hi MarkdownCodeBlock guifg=#ffffff ctermfg=15

hi MarkdownListMarker gui=none guifg=#44bc44 cterm=none ctermfg=2
hi default link MarkdownOrderedListMarker MarkdownListMarker

" Linting
" -------
hi YcmErrorSection gui=undercurl guisp=#ef8690 cterm=underline
hi YcmWarningSection gui=undercurl guisp=#b0aa00 cterm=underline
hi SyntasticError gui=undercurl guisp=#ef8690 cterm=underline
hi SyntasticWarning gui=undercurl guisp=#b0aa00 cterm=underline
hi SyntasticErrorSing guifg=#000000 guibg=#ef8690 ctermfg=0 ctermbg=1
hi SyntasticWarningSign guifg=#000000 guibg=#b0aa00 ctermfg=0 ctermbg=3
