" Name: Tempus Summer
" Description: Dark theme with colours inspired by summer evenings by the sea (WCAG AA compliant)
" Author: Protesilaos Stavrou (https://protesilaos.com)
" Meta: Created with the Tempus Themes Generator
" URL: https://gitlab.com/protesilaos/tempus-themes-generator

set background=dark
hi clear
if exists("syntax_on")
	syntax reset
endif
let g:colors_name = "tempus_summer"

" General
" -------
if exists("g:tempus_enforce_background_color")
	hi Normal guibg=#202c3d guifg=#a0abae ctermbg=0 ctermfg=15
else
	" NOTE the ctermbg=none is for terminals with transparency
	hi Normal guibg=#202c3d guifg=#a0abae ctermbg=none ctermfg=15
endif

hi Visual guibg=#a0abae guifg=#202c3d ctermbg=15 ctermfg=0
hi Search gui=underline,bold guibg=#39304f guifg=#a0abae cterm=underline,bold ctermbg=8 ctermfg=3
hi IncSearch gui=underline,bold guibg=#949cbf guifg=#202c3d term=none cterm=underline,bold ctermbg=7 ctermfg=0

hi StatusLine gui=none guibg=#a0abae guifg=#202c3d cterm=none ctermbg=15 ctermfg=0
hi StatusLineNC gui=none guibg=#39304f guifg=#949cbf cterm=none ctermbg=8 ctermfg=7
hi StatusLineTerm gui=none guibg=#4eb075 guifg=#202c3d cterm=none ctermbg=2 ctermfg=0
hi StatusLineTermNC gui=none guibg=#39304f guifg=#4eb075 cterm=none ctermbg=8 ctermfg=2

hi VertSplit gui=none cterm=none
hi TabLine gui=none guibg=#39304f guifg=#949cbf cterm=none ctermbg=8 ctermfg=7
hi TabLineSel gui=none guibg=#3dae9f guifg=#202c3d cterm=none ctermbg=6 ctermfg=0
hi TabLineFill gui=none cterm=none

hi Comment gui=italic guifg=#949cbf cterm=none ctermfg=7
hi Todo gui=bold guibg=#39304f guifg=#be981f cterm=bold ctermbg=8 ctermfg=11

hi Warning gui=none guibg=#ba9a0a guifg=#202c3d cterm=none ctermbg=3 ctermfg=0
hi WarningMsg gui=none guibg=#ba9a0a guifg=#202c3d cterm=none ctermbg=3 ctermfg=0
hi Error gui=none guibg=#fe6f70 guifg=#202c3d cterm=none ctermbg=1 ctermfg=0
hi ErrorMsg gui=none guibg=#fe6f70 guifg=#202c3d cterm=none ctermbg=1 ctermfg=0

hi MatchParen gui=underline,bold guibg=#39304f guifg=#949cbf cterm=underline,bold ctermbg=8 ctermfg=7

hi ToolbarLine guibg=#949cbf guifg=#202c3d term=none ctermbg=7 ctermfg=0
hi ToolbarButton gui=bold guibg=#949cbf guifg=#202c3d term=none cterm=bold ctermbg=7 ctermfg=0

hi WildMenu guibg=#202c3d guifg=#a0abae term=standout ctermbg=0 ctermfg=15

hi Terminal guibg=#202c3d guifg=#a0abae term=none ctermbg=0 ctermfg=15

" Constructs
" ----------
hi Constant guifg=#60a1e6 ctermfg=4
hi Number guifg=#60a1e6 ctermfg=4
hi Float guifg=#60a1e6 ctermfg=4
hi String guifg=#8599ef ctermfg=12

hi Function guifg=#d285ad ctermfg=5
hi Identifier guifg=#cc82d7 term=none ctermfg=13
hi Label guifg=#d285ad ctermfg=5
hi Tag guifg=#d285ad ctermfg=5
hi Keyword gui=bold guifg=#cc82d7 gui=bold ctermfg=13

hi Character gui=bold guifg=#2aacbf cterm=bold ctermfg=14

hi Type gui=none,bold guifg=#3dae9f term=none cterm=none,bold ctermfg=6
hi Boolean guifg=#3dae9f ctermfg=6
hi StorageClass guifg=#3dae9f ctermfg=6
hi Structure guifg=#3dae9f ctermfg=6
hi Typedef gui=bold guifg=#2aacbf cterm=bold ctermfg=14

hi Conditional gui=bold guifg=#4eb075 cterm=bold ctermfg=2
hi Statement gui=none guifg=#5baf4f cterm=none ctermfg=10
hi Repeat gui=bold guifg=#5baf4f cterm=bold ctermfg=10
hi Operator gui=bold guifg=#a0abae cterm=bold ctermfg=15
hi Exception gui=bold guifg=#fe6f70 cterm=bold ctermfg=1

hi Preproc gui=none guifg=#ec7f4f term=none cterm=none ctermfg=9
hi PreCondit gui=bold guifg=#ec7f4f cterm=bold ctermfg=9
hi Macro gui=bold guifg=#ec7f4f cterm=bold ctermfg=9
hi Include guifg=#ec7f4f ctermfg=9
hi Define guifg=#ec7f4f ctermfg=9

hi Title gui=bold guibg=#202c3d guifg=#3dae9f cterm=bold ctermbg=0 ctermfg=6

hi Delimeter gui=bold guifg=#d285ad cterm=bold ctermfg=5
hi Delimiter gui=bold guifg=#d285ad cterm=bold ctermfg=5
hi SpecialComment gui=bold guifg=#d285ad cterm=bold ctermfg=5

hi Debug guifg=#cc82d7 ctermfg=13

" Other
" -----
hi LineNr guibg=#39304f guifg=#949cbf term=none ctermbg=8 ctermfg=7
hi Cursor guibg=#a0abae guifg=#202c3d ctermbg=15 ctermfg=0
hi CursorLine gui=none guibg=#39304f term=none cterm=none ctermbg=8
hi CursorColumn gui=none guibg=#39304f term=none cterm=none ctermbg=8
hi CursorLineNr gui=bold guibg=#949cbf guifg=#202c3d cterm=bold ctermbg=7 ctermfg=0
hi ColorColumn guibg=#39304f guifg=#a0abae term=none ctermbg=8 ctermfg=15
hi SignColumn guibg=#39304f guifg=#949cbf term=none ctermbg=8 ctermfg=7

hi Folded guibg=#39304f guifg=#949cbf ctermbg=8 ctermfg=7
hi FoldColumn guibg=#39304f guifg=#949cbf ctermbg=8 ctermfg=7

hi Special gui=bold guifg=#be981f term=none cterm=bold ctermfg=11
hi SpecialKey gui=none guibg=#39304f guifg=#949cbf cterm=none ctermbg=8 ctermfg=7
hi SpecialChar gui=bold guifg=#be981f cterm=bold ctermfg=11
hi NonText gui=none guibg=#39304f guifg=#949cbf cterm=none ctermbg=8 ctermfg=7
hi EndOfBuffer gui=bold guifg=#949cbf cterm=bold ctermfg=7

hi Directory gui=none guifg=#4eb075 term=none cterm=none ctermfg=2
hi Question gui=bold guifg=#be981f cterm=bold ctermfg=11
hi MoreMsg guifg=#5baf4f ctermfg=10
hi ModeMsg gui=bold guifg=#4eb075 cterm=bold ctermfg=2

hi VimOption guifg=#d285ad ctermfg=5
hi VimGroup guifg=#d285ad ctermfg=5

hi Underlined gui=underline guifg=#a0abae cterm=underline ctermfg=15
hi Ignore guibg=#39304f guifg=#949cbf ctermbg=8 ctermfg=7
hi Conceal guibg=#949cbf guifg=#39304f ctermbg=7 ctermfg=8

hi SpellBad guibg=#fe6f70 guifg=#202c3d ctermbg=1 ctermfg=0
hi SpellCap guibg=#ba9a0a guifg=#202c3d ctermbg=3 ctermfg=0
hi SpellRare guibg=#cc82d7 guifg=#202c3d ctermbg=13 ctermfg=0
hi SpellLocal guibg=#2aacbf guifg=#202c3d ctermbg=14 ctermfg=0

hi Pmenu gui=italic guibg=#39304f guifg=#a0abae cterm=none ctermbg=8 ctermfg=15
hi PmenuSel gui=none,bold guibg=#949cbf guifg=#202c3d cterm=none,bold ctermbg=7 ctermfg=0
hi PmenuSbar guibg=#39304f ctermbg=8
hi PmenuThumb guibg=#949cbf ctermbg=7

" Terminal
" --------

if exists('*term_setansicolors')
	let g:terminal_ansi_colors = [
				\ "#202c3d",
				\ "#fe6f70",
				\ "#4eb075",
				\ "#ba9a0a",
				\ "#60a1e6",
				\ "#d285ad",
				\ "#3dae9f",
				\ "#949cbf",
				\ "#39304f",
				\ "#ec7f4f",
				\ "#5baf4f",
				\ "#be981f",
				\ "#8599ef",
				\ "#cc82d7",
				\ "#2aacbf",
				\ "#a0abae"
				\ ]
endif
if has('nvim')
	let g:terminal_color_0 = "#202c3d"
	let g:terminal_color_1 = "#fe6f70"
	let g:terminal_color_2 = "#4eb075"
	let g:terminal_color_3 = "#ba9a0a"
	let g:terminal_color_4 = "#60a1e6"
	let g:terminal_color_5 = "#d285ad"
	let g:terminal_color_6 = "#3dae9f"
	let g:terminal_color_7 = "#949cbf"
	let g:terminal_color_8 = "#39304f"
	let g:terminal_color_9 = "#ec7f4f"
	let g:terminal_color_10 = "#5baf4f"
	let g:terminal_color_11 = "#be981f"
	let g:terminal_color_12 = "#8599ef"
	let g:terminal_color_13 = "#cc82d7"
	let g:terminal_color_14 = "#2aacbf"
	let g:terminal_color_15 = "#a0abae"
endif

" Diffs
" -----
hi DiffAdd gui=bold guibg=#4eb075 guifg=#202c3d cterm=bold ctermbg=10 ctermfg=0
hi DiffDelete gui=none guibg=#fe6f70 guifg=#202c3d cterm=none ctermbg=9 ctermfg=0
hi DiffChange gui=bold guibg=#39304f guifg=#949cbf cterm=bold ctermbg=8 ctermfg=7
hi DiffText gui=bold guibg=#39304f guifg=#ec7f4f cterm=bold ctermbg=8 ctermfg=1

hi diffAdded guifg=#4eb075 ctermfg=2
hi diffRemoved guifg=#fe6f70 ctermfg=1
hi diffNewFile gui=none guifg=#60a1e6 ctermfg=4
hi diffFile gui=none guifg=#ba9a0a cterm=none ctermfg=3

hi GitGutterAdd guibg=#39304f guifg=#4eb075 ctermbg=8 ctermfg=2
hi GitGutterChange gui=bold guibg=#39304f guifg=#949cbf cterm=bold ctermbg=8 ctermfg=7
hi GitGutterDelete guibg=#39304f guifg=#fe6f70 ctermbg=8 ctermfg=1
hi GitGutterChangeDelete gui=bold guibg=#39304f guifg=#fe6f70 cterm=bold ctermbg=8 ctermfg=1

" Neomake
" -------
hi NeomakeError gui=none guibg=#fe6f70 guifg=#202c3d cterm=none ctermbg=1 ctermfg=0
hi NeomakeInfo gui=none guibg=#3dae9f guifg=#202c3d cterm=none ctermbg=6 ctermfg=0
hi NeomakeWarning gui=none guibg=#ba9a0a guifg=#202c3d cterm=none ctermbg=3 ctermfg=0
hi NeomakeMessage gui=none guibg=#be981f guifg=#202c3d cterm=none ctermbg=11 ctermfg=0

hi NeomakeVirtualtextInfoDefault guifg=#2aacbf ctermfg=14
hi NeomakeVirtualtextMessageDefault guifg=#be981f ctermfg=11
hi NeomakeVirtualtextWarningDefault guifg=#ba9a0a ctermfg=3
hi NeomakeVirtualtextErrorDefault guifg=#fe6f70 ctermfg=1

hi NeomakeStatusGood gui=none guibg=#4eb075 guifg=#202c3d cterm=none ctermbg=2 ctermfg=0
hi NeomakeStatusGoodNC gui=none guibg=#39304f guifg=#4eb075 cterm=none ctermbg=8 ctermfg=2

hi NeomakeStatColorDefault gui=none guibg=#60a1e6 guifg=#202c3d cterm=none ctermbg=4 ctermfg=0
hi NeomakeStatColorTypeE gui=none guibg=#fe6f70 guifg=#202c3d cterm=none ctermbg=1 ctermfg=0
hi NeomakeStatColorTypeW gui=none guibg=#ba9a0a guifg=#202c3d cterm=none ctermbg=3 ctermfg=0

" Markdown
" --------
hi MarkdownRule gui=bold guibg=#39304f guifg=#5baf4f cterm=bold ctermbg=8 ctermfg=10

hi MarkdownHeading gui=bold guifg=#a0abae cterm=bold ctermfg=15
hi default link MarkdownH1 MarkdownHeading
hi default link MarkdownH2 MarkdownHeading
hi default link MarkdownH3 MarkdownHeading
hi default link MarkdownH4 MarkdownHeading
hi default link MarkdownH5 MarkdownHeading
hi default link MarkdownH6 MarkdownHeading
hi default link MarkdownHeadingDelimiter MarkdownHeading
hi default link MarkdownHeadingRule MarkdownHeading

hi MarkdownBold gui=bold guifg=#ec7f4f cterm=bold ctermfg=9
hi default link MarkdownBoldDelimiter MarkdownBold

hi MarkdownItalic gui=italic guifg=#ba9a0a cterm=none ctermfg=3
hi default link MarkdownItalicDelimiter MarkdownItalic

hi MarkdownUrl gui=underline guifg=#60a1e6 cterm=underline ctermfg=4
hi MarkdownLinkText gui=none guifg=#8599ef cterm=none ctermfg=12
hi MarkdownLinkDelimiter gui=bold guifg=#a0abae cterm=bold ctermfg=15
hi default link MarkdownLinkTextDelimiter MarkdownLinkDelimiter

hi MarkdownCode guifg=#d285ad ctermfg=5
hi default link MarkdownCodeDelimiter MarkdownCode

hi MarkdownCodeBlock guifg=#a0abae ctermfg=15

hi MarkdownListMarker gui=none guifg=#4eb075 cterm=none ctermfg=2
hi default link MarkdownOrderedListMarker MarkdownListMarker

" Linting
" -------
hi YcmErrorSection gui=undercurl guisp=#fe6f70 cterm=underline
hi YcmWarningSection gui=undercurl guisp=#ba9a0a cterm=underline
hi SyntasticError gui=undercurl guisp=#fe6f70 cterm=underline
hi SyntasticWarning gui=undercurl guisp=#ba9a0a cterm=underline
hi SyntasticErrorSing guifg=#202c3d guibg=#fe6f70 ctermfg=0 ctermbg=1
hi SyntasticWarningSign guifg=#202c3d guibg=#ba9a0a ctermfg=0 ctermbg=3
