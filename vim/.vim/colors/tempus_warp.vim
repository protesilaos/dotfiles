" Name: Tempus Warp
" Description: Dark theme with a vibrant palette (WCAG AA compliant)
" Author: Protesilaos Stavrou (https://protesilaos.com)
" Meta: Created with the Tempus Themes Generator
" URL: https://gitlab.com/protesilaos/tempus-themes-generator

set background=dark
hi clear
if exists("syntax_on")
	syntax reset
endif
let g:colors_name = "tempus_warp"

" General
" -------
if exists("g:tempus_enforce_background_color")
	hi Normal guibg=#001514 guifg=#a29fa0 ctermbg=0 ctermfg=15
else
	" NOTE the ctermbg=none is for terminals with transparency
	hi Normal guibg=#001514 guifg=#a29fa0 ctermbg=none ctermfg=15
endif

hi Visual guibg=#a29fa0 guifg=#001514 ctermbg=15 ctermfg=0
hi Search gui=underline,bold guibg=#261c2c guifg=#a29fa0 cterm=underline,bold ctermbg=8 ctermfg=3
hi IncSearch gui=underline,bold guibg=#968282 guifg=#001514 term=none cterm=underline,bold ctermbg=7 ctermfg=0

hi StatusLine gui=none guibg=#a29fa0 guifg=#001514 cterm=none ctermbg=15 ctermfg=0
hi StatusLineNC gui=none guibg=#261c2c guifg=#968282 cterm=none ctermbg=8 ctermfg=7
hi StatusLineTerm gui=none guibg=#169c16 guifg=#001514 cterm=none ctermbg=2 ctermfg=0
hi StatusLineTermNC gui=none guibg=#261c2c guifg=#169c16 cterm=none ctermbg=8 ctermfg=2

hi VertSplit gui=none cterm=none
hi TabLine gui=none guibg=#261c2c guifg=#968282 cterm=none ctermbg=8 ctermfg=7
hi TabLineSel gui=none guibg=#009880 guifg=#001514 cterm=none ctermbg=6 ctermfg=0
hi TabLineFill gui=none cterm=none

hi Comment gui=italic guifg=#968282 cterm=none ctermfg=7
hi Todo gui=bold guibg=#261c2c guifg=#ba8a00 cterm=bold ctermbg=8 ctermfg=11

hi Warning gui=none guibg=#9f8500 guifg=#001514 cterm=none ctermbg=3 ctermfg=0
hi WarningMsg gui=none guibg=#9f8500 guifg=#001514 cterm=none ctermbg=3 ctermfg=0
hi Error gui=none guibg=#ff3737 guifg=#001514 cterm=none ctermbg=1 ctermfg=0
hi ErrorMsg gui=none guibg=#ff3737 guifg=#001514 cterm=none ctermbg=1 ctermfg=0

hi MatchParen gui=underline,bold guibg=#261c2c guifg=#968282 cterm=underline,bold ctermbg=8 ctermfg=7

hi ToolbarLine guibg=#968282 guifg=#001514 term=none ctermbg=7 ctermfg=0
hi ToolbarButton gui=bold guibg=#968282 guifg=#001514 term=none cterm=bold ctermbg=7 ctermfg=0

hi WildMenu guibg=#001514 guifg=#a29fa0 term=standout ctermbg=0 ctermfg=15

hi Terminal guibg=#001514 guifg=#a29fa0 term=none ctermbg=0 ctermfg=15

" Constructs
" ----------
hi Constant guifg=#5781ef ctermfg=4
hi Number guifg=#5781ef ctermfg=4
hi Float guifg=#5781ef ctermfg=4
hi String guifg=#8887f0 ctermfg=12

hi Function guifg=#da4ebf ctermfg=5
hi Identifier guifg=#d85cf2 term=none ctermfg=13
hi Label guifg=#da4ebf ctermfg=5
hi Tag guifg=#da4ebf ctermfg=5
hi Keyword gui=bold guifg=#d85cf2 gui=bold ctermfg=13

hi Character gui=bold guifg=#1da1af cterm=bold ctermfg=14

hi Type gui=none,bold guifg=#009880 term=none cterm=none,bold ctermfg=6
hi Boolean guifg=#009880 ctermfg=6
hi StorageClass guifg=#009880 ctermfg=6
hi Structure guifg=#009880 ctermfg=6
hi Typedef gui=bold guifg=#1da1af cterm=bold ctermfg=14

hi Conditional gui=bold guifg=#169c16 cterm=bold ctermfg=2
hi Statement gui=none guifg=#3aa73a cterm=none ctermfg=10
hi Repeat gui=bold guifg=#3aa73a cterm=bold ctermfg=10
hi Operator gui=bold guifg=#a29fa0 cterm=bold ctermfg=15
hi Exception gui=bold guifg=#ff3737 cterm=bold ctermfg=1

hi Preproc gui=none guifg=#F0681A term=none cterm=none ctermfg=9
hi PreCondit gui=bold guifg=#F0681A cterm=bold ctermfg=9
hi Macro gui=bold guifg=#F0681A cterm=bold ctermfg=9
hi Include guifg=#F0681A ctermfg=9
hi Define guifg=#F0681A ctermfg=9

hi Title gui=bold guibg=#001514 guifg=#009880 cterm=bold ctermbg=0 ctermfg=6

hi Delimeter gui=bold guifg=#da4ebf cterm=bold ctermfg=5
hi Delimiter gui=bold guifg=#da4ebf cterm=bold ctermfg=5
hi SpecialComment gui=bold guifg=#da4ebf cterm=bold ctermfg=5

hi Debug guifg=#d85cf2 ctermfg=13

" Other
" -----
hi LineNr guibg=#261c2c guifg=#968282 term=none ctermbg=8 ctermfg=7
hi Cursor guibg=#a29fa0 guifg=#001514 ctermbg=15 ctermfg=0
hi CursorLine gui=none guibg=#261c2c term=none cterm=none ctermbg=8
hi CursorColumn gui=none guibg=#261c2c term=none cterm=none ctermbg=8
hi CursorLineNr gui=bold guibg=#968282 guifg=#001514 cterm=bold ctermbg=7 ctermfg=0
hi ColorColumn guibg=#261c2c guifg=#a29fa0 term=none ctermbg=8 ctermfg=15
hi SignColumn guibg=#261c2c guifg=#968282 term=none ctermbg=8 ctermfg=7

hi Folded guibg=#261c2c guifg=#968282 ctermbg=8 ctermfg=7
hi FoldColumn guibg=#261c2c guifg=#968282 ctermbg=8 ctermfg=7

hi Special gui=bold guifg=#ba8a00 term=none cterm=bold ctermfg=11
hi SpecialKey gui=none guibg=#261c2c guifg=#968282 cterm=none ctermbg=8 ctermfg=7
hi SpecialChar gui=bold guifg=#ba8a00 cterm=bold ctermfg=11
hi NonText gui=none guibg=#261c2c guifg=#968282 cterm=none ctermbg=8 ctermfg=7
hi EndOfBuffer gui=bold guifg=#968282 cterm=bold ctermfg=7

hi Directory gui=none guifg=#169c16 term=none cterm=none ctermfg=2
hi Question gui=bold guifg=#ba8a00 cterm=bold ctermfg=11
hi MoreMsg guifg=#3aa73a ctermfg=10
hi ModeMsg gui=bold guifg=#169c16 cterm=bold ctermfg=2

hi VimOption guifg=#da4ebf ctermfg=5
hi VimGroup guifg=#da4ebf ctermfg=5

hi Underlined gui=underline guifg=#a29fa0 cterm=underline ctermfg=15
hi Ignore guibg=#261c2c guifg=#968282 ctermbg=8 ctermfg=7
hi Conceal guibg=#968282 guifg=#261c2c ctermbg=7 ctermfg=8

hi SpellBad guibg=#ff3737 guifg=#001514 ctermbg=1 ctermfg=0
hi SpellCap guibg=#9f8500 guifg=#001514 ctermbg=3 ctermfg=0
hi SpellRare guibg=#d85cf2 guifg=#001514 ctermbg=13 ctermfg=0
hi SpellLocal guibg=#1da1af guifg=#001514 ctermbg=14 ctermfg=0

hi Pmenu gui=italic guibg=#261c2c guifg=#a29fa0 cterm=none ctermbg=8 ctermfg=15
hi PmenuSel gui=none,bold guibg=#968282 guifg=#001514 cterm=none,bold ctermbg=7 ctermfg=0
hi PmenuSbar guibg=#261c2c ctermbg=8
hi PmenuThumb guibg=#968282 ctermbg=7

" Terminal
" --------

if exists('*term_setansicolors')
	let g:terminal_ansi_colors = [
				\ "#001514",
				\ "#ff3737",
				\ "#169c16",
				\ "#9f8500",
				\ "#5781ef",
				\ "#da4ebf",
				\ "#009880",
				\ "#968282",
				\ "#261c2c",
				\ "#F0681A",
				\ "#3aa73a",
				\ "#ba8a00",
				\ "#8887f0",
				\ "#d85cf2",
				\ "#1da1af",
				\ "#a29fa0"
				\ ]
endif
if has('nvim')
	let g:terminal_color_0 = "#001514"
	let g:terminal_color_1 = "#ff3737"
	let g:terminal_color_2 = "#169c16"
	let g:terminal_color_3 = "#9f8500"
	let g:terminal_color_4 = "#5781ef"
	let g:terminal_color_5 = "#da4ebf"
	let g:terminal_color_6 = "#009880"
	let g:terminal_color_7 = "#968282"
	let g:terminal_color_8 = "#261c2c"
	let g:terminal_color_9 = "#F0681A"
	let g:terminal_color_10 = "#3aa73a"
	let g:terminal_color_11 = "#ba8a00"
	let g:terminal_color_12 = "#8887f0"
	let g:terminal_color_13 = "#d85cf2"
	let g:terminal_color_14 = "#1da1af"
	let g:terminal_color_15 = "#a29fa0"
endif

" Diffs
" -----
hi DiffAdd gui=bold guibg=#169c16 guifg=#001514 cterm=bold ctermbg=10 ctermfg=0
hi DiffDelete gui=none guibg=#ff3737 guifg=#001514 cterm=none ctermbg=9 ctermfg=0
hi DiffChange gui=bold guibg=#261c2c guifg=#968282 cterm=bold ctermbg=8 ctermfg=7
hi DiffText gui=bold guibg=#261c2c guifg=#F0681A cterm=bold ctermbg=8 ctermfg=1

hi diffAdded guifg=#169c16 ctermfg=2
hi diffRemoved guifg=#ff3737 ctermfg=1
hi diffNewFile gui=none guifg=#5781ef ctermfg=4
hi diffFile gui=none guifg=#9f8500 cterm=none ctermfg=3

hi GitGutterAdd guibg=#261c2c guifg=#169c16 ctermbg=8 ctermfg=2
hi GitGutterChange gui=bold guibg=#261c2c guifg=#968282 cterm=bold ctermbg=8 ctermfg=7
hi GitGutterDelete guibg=#261c2c guifg=#ff3737 ctermbg=8 ctermfg=1
hi GitGutterChangeDelete gui=bold guibg=#261c2c guifg=#ff3737 cterm=bold ctermbg=8 ctermfg=1

" Neomake
" -------
hi NeomakeError gui=none guibg=#ff3737 guifg=#001514 cterm=none ctermbg=1 ctermfg=0
hi NeomakeInfo gui=none guibg=#009880 guifg=#001514 cterm=none ctermbg=6 ctermfg=0
hi NeomakeWarning gui=none guibg=#9f8500 guifg=#001514 cterm=none ctermbg=3 ctermfg=0
hi NeomakeMessage gui=none guibg=#ba8a00 guifg=#001514 cterm=none ctermbg=11 ctermfg=0

hi NeomakeVirtualtextInfoDefault guifg=#1da1af ctermfg=14
hi NeomakeVirtualtextMessageDefault guifg=#ba8a00 ctermfg=11
hi NeomakeVirtualtextWarningDefault guifg=#9f8500 ctermfg=3
hi NeomakeVirtualtextErrorDefault guifg=#ff3737 ctermfg=1

hi NeomakeStatusGood gui=none guibg=#169c16 guifg=#001514 cterm=none ctermbg=2 ctermfg=0
hi NeomakeStatusGoodNC gui=none guibg=#261c2c guifg=#169c16 cterm=none ctermbg=8 ctermfg=2

hi NeomakeStatColorDefault gui=none guibg=#5781ef guifg=#001514 cterm=none ctermbg=4 ctermfg=0
hi NeomakeStatColorTypeE gui=none guibg=#ff3737 guifg=#001514 cterm=none ctermbg=1 ctermfg=0
hi NeomakeStatColorTypeW gui=none guibg=#9f8500 guifg=#001514 cterm=none ctermbg=3 ctermfg=0

" Markdown
" --------
hi MarkdownRule gui=bold guibg=#261c2c guifg=#3aa73a cterm=bold ctermbg=8 ctermfg=10

hi MarkdownHeading gui=bold guifg=#a29fa0 cterm=bold ctermfg=15
hi default link MarkdownH1 MarkdownHeading
hi default link MarkdownH2 MarkdownHeading
hi default link MarkdownH3 MarkdownHeading
hi default link MarkdownH4 MarkdownHeading
hi default link MarkdownH5 MarkdownHeading
hi default link MarkdownH6 MarkdownHeading
hi default link MarkdownHeadingDelimiter MarkdownHeading
hi default link MarkdownHeadingRule MarkdownHeading

hi MarkdownBold gui=bold guifg=#F0681A cterm=bold ctermfg=9
hi default link MarkdownBoldDelimiter MarkdownBold

hi MarkdownItalic gui=italic guifg=#9f8500 cterm=none ctermfg=3
hi default link MarkdownItalicDelimiter MarkdownItalic

hi MarkdownUrl gui=underline guifg=#5781ef cterm=underline ctermfg=4
hi MarkdownLinkText gui=none guifg=#8887f0 cterm=none ctermfg=12
hi MarkdownLinkDelimiter gui=bold guifg=#a29fa0 cterm=bold ctermfg=15
hi default link MarkdownLinkTextDelimiter MarkdownLinkDelimiter

hi MarkdownCode guifg=#da4ebf ctermfg=5
hi default link MarkdownCodeDelimiter MarkdownCode

hi MarkdownCodeBlock guifg=#a29fa0 ctermfg=15

hi MarkdownListMarker gui=none guifg=#169c16 cterm=none ctermfg=2
hi default link MarkdownOrderedListMarker MarkdownListMarker

" Linting
" -------
hi YcmErrorSection gui=undercurl guisp=#ff3737 cterm=underline
hi YcmWarningSection gui=undercurl guisp=#9f8500 cterm=underline
hi SyntasticError gui=undercurl guisp=#ff3737 cterm=underline
hi SyntasticWarning gui=undercurl guisp=#9f8500 cterm=underline
hi SyntasticErrorSing guifg=#001514 guibg=#ff3737 ctermfg=0 ctermbg=1
hi SyntasticWarningSign guifg=#001514 guibg=#9f8500 ctermfg=0 ctermbg=3
