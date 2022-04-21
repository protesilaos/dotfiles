" vi: ft=vim
" Name: modus_operandi
" Description: Vim port of modus-operandi (Modus themes for Emacs)
" Author: Protesilaos Stavrou, <https://protesilaos.com>
" Meta: Created with the modus-themes-exporter.el library
" URL: https://git.sr.ht/~protesilaos/modus-themes

set background=light
hi clear
if exists('syntax_on')
	syntax reset
endif
let g:colors_name = 'modus_operandi'

" General
" -------
if exists('g:modus_themes_enforce_background_color')
	hi Normal guibg=#ffffff guifg=#000000 ctermbg=15 ctermfg=0
else
	" NOTE the ctermbg=none is for terminals with transparency
	hi Normal guibg=#ffffff guifg=#000000 ctermbg=none ctermfg=0
endif

hi Visual guibg=#000000 guifg=#ffffff ctermbg=0 ctermfg=15
hi Search gui=underline,bold guibg=#f0f0f0 guifg=#000000 cterm=underline,bold ctermbg=7 ctermfg=3
hi IncSearch gui=underline,bold guibg=#505050 guifg=#ffffff term=none cterm=underline,bold ctermbg=8 ctermfg=15

hi StatusLine gui=none guibg=#000000 guifg=#ffffff cterm=none ctermbg=0 ctermfg=15
hi StatusLineNC gui=none guibg=#f0f0f0 guifg=#505050 cterm=none ctermbg=7 ctermfg=8
hi StatusLineTerm gui=none guibg=#005e00 guifg=#ffffff cterm=none ctermbg=2 ctermfg=15
hi StatusLineTermNC gui=none guibg=#f0f0f0 guifg=#005e00 cterm=none ctermbg=7 ctermfg=2

hi VertSplit gui=none cterm=none
hi TabLine gui=none guibg=#f0f0f0 guifg=#505050 cterm=none ctermbg=7 ctermfg=8
hi TabLineSel gui=none guibg=#00538b guifg=#ffffff cterm=none ctermbg=6 ctermfg=15
hi TabLineFill gui=none cterm=none

hi Comment gui=italic guifg=#505050 cterm=none ctermfg=8
hi Todo guibg=#f8f8f8 guifg=#70480f ctermbg=15 ctermfg=11

hi Warning gui=none guibg=#813e00 guifg=#ffffff cterm=none ctermbg=3 ctermfg=15
hi WarningMsg gui=none guibg=#813e00 guifg=#ffffff cterm=none ctermbg=3 ctermfg=15
hi Error gui=none guibg=#a60000 guifg=#ffffff cterm=none ctermbg=1 ctermfg=15
hi ErrorMsg gui=none guibg=#a60000 guifg=#ffffff cterm=none ctermbg=1 ctermfg=15

hi MatchParen gui=underline,bold guibg=#f0f0f0 guifg=#505050 cterm=underline,bold ctermbg=7 ctermfg=8

hi ToolbarLine guibg=#505050 guifg=#ffffff term=none ctermbg=8 ctermfg=15
hi ToolbarButton gui=bold guibg=#505050 guifg=#ffffff term=none cterm=bold ctermbg=8 ctermfg=15

hi WildMenu guibg=#ffffff guifg=#000000 term=standout ctermbg=15 ctermfg=0

hi Terminal guibg=#ffffff guifg=#000000 term=none ctermbg=15 ctermfg=0

" Constructs
" ----------
hi Constant guifg=#0000c0 ctermfg=4
hi Boolean guifg=#005e00 ctermfg=2
hi Number guifg=#0000c0 ctermfg=4
hi Float guifg=#0000c0 ctermfg=4
hi String guifg=#2544bb ctermfg=12

hi Function guifg=#721045 ctermfg=5
hi Identifier gui=none guifg=#00538b cterm=none ctermfg=6
hi Label guifg=#721045 ctermfg=5
hi Tag guifg=#721045 ctermfg=5
hi Keyword gui=none guifg=#8f0075 cterm=none ctermfg=5

hi Character gui=bold guifg=#005a5f cterm=bold ctermfg=14

hi Type gui=none,bold guifg=#00538b term=none cterm=none,bold ctermfg=6
hi StorageClass guifg=#00538b ctermfg=6
hi Structure guifg=#00538b ctermfg=6
hi Typedef gui=bold guifg=#005a5f cterm=bold ctermfg=14

hi Conditional gui=none guifg=#5317ac cterm=none ctermfg=13
hi Statement gui=none guifg=#5317ac cterm=none ctermfg=13
hi Repeat gui=bold guifg=#8f0075 cterm=bold ctermfg=5
hi Operator gui=none guifg=#000000 cterm=none ctermfg=0
hi Exception gui=bold guifg=#813e00 cterm=bold ctermfg=2

hi Preproc gui=none guifg=#972500 term=none cterm=none ctermfg=9
hi PreCondit gui=bold guifg=#972500 cterm=bold ctermfg=9
hi Macro gui=bold guifg=#8f0075 cterm=bold ctermfg=5
hi Include guifg=#972500 ctermfg=9
hi Define guifg=#972500 ctermfg=9

hi Title gui=bold guibg=#ffffff guifg=#00538b cterm=bold ctermbg=15 ctermfg=6

hi Delimiter gui=none guifg=#000000 cterm=none ctermfg=0
hi SpecialComment gui=bold guifg=#863927 cterm=bold ctermfg=3

hi Debug guifg=#8f0075 ctermfg=13

" Other
" -----
hi LineNr guibg=#f0f0f0 guifg=#505050 term=none ctermbg=7 ctermfg=8
hi Cursor guibg=#000000 guifg=#ffffff ctermbg=0 ctermfg=15
hi CursorLine gui=none guibg=#f0f0f0 term=none cterm=none ctermbg=7
hi CursorColumn gui=none guibg=#f0f0f0 term=none cterm=none ctermbg=7
hi CursorLineNr gui=bold guibg=#505050 guifg=#ffffff cterm=bold ctermbg=8 ctermfg=15
hi ColorColumn guibg=#f0f0f0 guifg=#000000 term=none ctermbg=7 ctermfg=0
hi SignColumn guibg=#f0f0f0 guifg=#505050 term=none ctermbg=7 ctermfg=8

hi Folded guibg=#f0f0f0 guifg=#505050 ctermbg=7 ctermfg=8
hi FoldColumn guibg=#f0f0f0 guifg=#505050 ctermbg=7 ctermfg=8

hi Special gui=none guifg=#145c33 term=none cterm=none ctermfg=2
hi SpecialKey gui=none guibg=#f0f0f0 guifg=#505050 cterm=none ctermbg=7 ctermfg=8
hi SpecialChar gui=bold guifg=#70480f cterm=bold ctermfg=11
hi NonText gui=none guibg=#f0f0f0 guifg=#505050 cterm=none ctermbg=7 ctermfg=8
hi EndOfBuffer gui=bold guifg=#505050 cterm=bold ctermfg=8

hi Directory gui=none guifg=#005e00 term=none cterm=none ctermfg=2
hi Question gui=bold guifg=#70480f cterm=bold ctermfg=11
hi MoreMsg guifg=#315b00 ctermfg=10
hi ModeMsg gui=bold guifg=#005e00 cterm=bold ctermfg=2

hi VimOption guifg=#30517f ctermfg=6
hi VimGroup guifg=#30517f ctermfg=5

hi Underlined gui=underline guifg=#000000 cterm=underline ctermfg=0
hi Ignore guibg=#f0f0f0 guifg=#505050 ctermbg=7 ctermfg=8
hi Conceal guibg=#505050 guifg=#f0f0f0 ctermbg=8 ctermfg=7

hi SpellBad guibg=#a60000 guifg=#ffffff ctermbg=1 ctermfg=15
hi SpellCap guibg=#813e00 guifg=#ffffff ctermbg=3 ctermfg=15
hi SpellRare guibg=#5317ac guifg=#ffffff ctermbg=13 ctermfg=15
hi SpellLocal guibg=#005a5f guifg=#ffffff ctermbg=14 ctermfg=15

hi Pmenu gui=italic guibg=#f0f0f0 guifg=#000000 cterm=none ctermbg=7 ctermfg=0
hi PmenuSel gui=none,bold guibg=#505050 guifg=#ffffff cterm=none,bold ctermbg=8 ctermfg=15
hi PmenuSbar guibg=#f0f0f0 ctermbg=7
hi PmenuThumb guibg=#505050 ctermbg=8

" Terminal
" --------

if exists('*term_setansicolors')
	let g:terminal_ansi_colors = [
				\ '#000000',
				\ '#a60000',
				\ '#005e00',
				\ '#813e00',
				\ '#0031a9',
				\ '#721045',
				\ '#00538b',
				\ '#bfbfbf',
				\ '#595959',
				\ '#972500',
				\ '#315b00',
				\ '#70480f',
				\ '#2544bb',
				\ '#5317ac',
				\ '#005a5f',
				\ '#ffffff'
				\ ]
endif
if has('nvim')
	let g:terminal_color_0 = '#000000'
	let g:terminal_color_1 = '#a60000'
	let g:terminal_color_2 = '#005e00'
	let g:terminal_color_3 = '#813e00'
	let g:terminal_color_4 = '#0031a9'
	let g:terminal_color_5 = '#721045'
	let g:terminal_color_6 = '#00538b'
	let g:terminal_color_7 = '#bfbfbf'
	let g:terminal_color_8 = '#595959'
	let g:terminal_color_9 = '#972500'
	let g:terminal_color_10 = '#315b00'
	let g:terminal_color_11 = '#70480f'
	let g:terminal_color_12 = '#2544bb'
	let g:terminal_color_13 = '#5317ac'
	let g:terminal_color_14 = '#005a5f'
	let g:terminal_color_15 = '#ffffff'
endif

" Diffs
" -----
hi DiffAdd gui=bold guibg=#005e00 guifg=#ffffff cterm=bold ctermbg=10 ctermfg=15
hi DiffDelete gui=none guibg=#a60000 guifg=#ffffff cterm=none ctermbg=9 ctermfg=15
hi DiffChange gui=bold guibg=#f0f0f0 guifg=#505050 cterm=bold ctermbg=7 ctermfg=8
hi DiffText gui=bold guibg=#f0f0f0 guifg=#972500 cterm=bold ctermbg=7 ctermfg=1

hi diffAdded guifg=#005e00 ctermfg=2
hi diffRemoved guifg=#a60000 ctermfg=1
hi diffNewFile gui=none guifg=#0031a9 ctermfg=4
hi diffFile gui=none guifg=#813e00 cterm=none ctermfg=3

hi GitGutterAdd guibg=#f0f0f0 guifg=#005e00 ctermbg=7 ctermfg=2
hi GitGutterChange gui=bold guibg=#f0f0f0 guifg=#505050 cterm=bold ctermbg=7 ctermfg=8
hi GitGutterDelete guibg=#f0f0f0 guifg=#a60000 ctermbg=7 ctermfg=1
hi GitGutterChangeDelete gui=bold guibg=#f0f0f0 guifg=#a60000 cterm=bold ctermbg=7 ctermfg=1

" Neomake
" -------
hi NeomakeError gui=none guibg=#a60000 guifg=#ffffff cterm=none ctermbg=1 ctermfg=15
hi NeomakeInfo gui=none guibg=#00538b guifg=#ffffff cterm=none ctermbg=6 ctermfg=15
hi NeomakeWarning gui=none guibg=#813e00 guifg=#ffffff cterm=none ctermbg=3 ctermfg=15
hi NeomakeMessage gui=none guibg=#70480f guifg=#ffffff cterm=none ctermbg=11 ctermfg=15

hi NeomakeVirtualtextInfoDefault guifg=#005a5f ctermfg=14
hi NeomakeVirtualtextMessageDefault guifg=#70480f ctermfg=11
hi NeomakeVirtualtextWarningDefault guifg=#813e00 ctermfg=3
hi NeomakeVirtualtextErrorDefault guifg=#a60000 ctermfg=1

hi NeomakeStatusGood gui=none guibg=#005e00 guifg=#ffffff cterm=none ctermbg=2 ctermfg=15
hi NeomakeStatusGoodNC gui=none guibg=#f0f0f0 guifg=#005e00 cterm=none ctermbg=7 ctermfg=2

hi NeomakeStatColorDefault gui=none guibg=#0031a9 guifg=#ffffff cterm=none ctermbg=4 ctermfg=15
hi NeomakeStatColorTypeE gui=none guibg=#a60000 guifg=#ffffff cterm=none ctermbg=1 ctermfg=15
hi NeomakeStatColorTypeW gui=none guibg=#813e00 guifg=#ffffff cterm=none ctermbg=3 ctermfg=15

" Markdown
" --------
hi MarkdownRule gui=bold guibg=#f0f0f0 guifg=#315b00 cterm=bold ctermbg=7 ctermfg=10

hi MarkdownHeading gui=bold guifg=#000000 cterm=bold ctermfg=0
hi default link MarkdownH1 MarkdownHeading
hi default link MarkdownH2 MarkdownHeading
hi default link MarkdownH3 MarkdownHeading
hi default link MarkdownH4 MarkdownHeading
hi default link MarkdownH5 MarkdownHeading
hi default link MarkdownH6 MarkdownHeading
hi default link MarkdownHeadingDelimiter MarkdownHeading
hi default link MarkdownHeadingRule MarkdownHeading

hi MarkdownBold gui=bold guifg=#972500 cterm=bold ctermfg=9
hi default link MarkdownBoldDelimiter MarkdownBold

hi MarkdownItalic gui=italic guifg=#813e00 cterm=none ctermfg=3
hi default link MarkdownItalicDelimiter MarkdownItalic

hi MarkdownUrl gui=underline guifg=#0031a9 cterm=underline ctermfg=4
hi MarkdownLinkText gui=none guifg=#2544bb cterm=none ctermfg=12
hi MarkdownLinkDelimiter gui=bold guifg=#000000 cterm=bold ctermfg=0
hi default link MarkdownLinkTextDelimiter MarkdownLinkDelimiter

hi MarkdownCode guifg=#721045 ctermfg=5
hi default link MarkdownCodeDelimiter MarkdownCode

hi MarkdownCodeBlock guifg=#000000 ctermfg=0

hi MarkdownListMarker gui=none guifg=#005e00 cterm=none ctermfg=2
hi default link MarkdownOrderedListMarker MarkdownListMarker

" Linting
" -------
hi YcmErrorSection gui=undercurl guisp=#9f004f cterm=underline
hi YcmWarningSection gui=undercurl guisp=#604f0f cterm=underline
hi SyntasticError gui=undercurl guisp=#9f004f cterm=underline
hi SyntasticWarning gui=undercurl guisp=#604f0f cterm=underline
hi SyntasticErrorSing guifg=#ffffff guibg=#9f004f ctermfg=15 ctermbg=1
hi SyntasticWarningSign guifg=#ffffff guibg=#604f0f ctermfg=15 ctermbg=3
