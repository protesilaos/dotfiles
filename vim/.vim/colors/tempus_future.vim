" Name: Tempus Future
" Description: Dark theme with colours inspired by concept art of outer space (WCAG AAA compliant)
" Author: Protesilaos Stavrou (https://protesilaos.com)
" Meta: Created with the Tempus Themes Generator
" URL: https://gitlab.com/protesilaos/tempus-themes-generator

set background=dark
hi clear
if exists("syntax_on")
	syntax reset
endif
let g:colors_name = "tempus_future"

" General
" -------
if exists("g:tempus_enforce_background_color")
	hi Normal guibg=#090a18 guifg=#b4abac ctermbg=0 ctermfg=15
else
	" NOTE the ctermbg=none is for terminals with transparency
	hi Normal guibg=#090a18 guifg=#b4abac ctermbg=none ctermfg=15
endif

hi Visual guibg=#b4abac guifg=#090a18 ctermbg=15 ctermfg=0
hi Search gui=underline,bold guibg=#2b1329 guifg=#b4abac cterm=underline,bold ctermbg=8 ctermfg=3
hi IncSearch gui=underline,bold guibg=#a7a2c4 guifg=#090a18 term=none cterm=underline,bold ctermbg=7 ctermfg=0

hi StatusLine gui=none guibg=#b4abac guifg=#090a18 cterm=none ctermbg=15 ctermfg=0
hi StatusLineNC gui=none guibg=#2b1329 guifg=#a7a2c4 cterm=none ctermbg=8 ctermfg=7
hi StatusLineTerm gui=none guibg=#6aba39 guifg=#090a18 cterm=none ctermbg=2 ctermfg=0
hi StatusLineTermNC gui=none guibg=#2b1329 guifg=#6aba39 cterm=none ctermbg=8 ctermfg=2

hi VertSplit gui=none cterm=none
hi TabLine gui=none guibg=#2b1329 guifg=#a7a2c4 cterm=none ctermbg=8 ctermfg=7
hi TabLineSel gui=none guibg=#2ab7bb guifg=#090a18 cterm=none ctermbg=6 ctermfg=0
hi TabLineFill gui=none cterm=none

hi Comment gui=italic guifg=#a7a2c4 cterm=none ctermfg=7
hi Todo gui=bold guibg=#2b1329 guifg=#de9b1d cterm=bold ctermbg=8 ctermfg=11

hi Warning gui=none guibg=#bfa51a guifg=#090a18 cterm=none ctermbg=3 ctermfg=0
hi WarningMsg gui=none guibg=#bfa51a guifg=#090a18 cterm=none ctermbg=3 ctermfg=0
hi Error gui=none guibg=#ff7e8f guifg=#090a18 cterm=none ctermbg=1 ctermfg=0
hi ErrorMsg gui=none guibg=#ff7e8f guifg=#090a18 cterm=none ctermbg=1 ctermfg=0

hi MatchParen gui=underline,bold guibg=#2b1329 guifg=#a7a2c4 cterm=underline,bold ctermbg=8 ctermfg=7

hi ToolbarLine guibg=#a7a2c4 guifg=#090a18 term=none ctermbg=7 ctermfg=0
hi ToolbarButton gui=bold guibg=#a7a2c4 guifg=#090a18 term=none cterm=bold ctermbg=7 ctermfg=0

hi WildMenu guibg=#090a18 guifg=#b4abac term=standout ctermbg=0 ctermfg=15

hi Terminal guibg=#090a18 guifg=#b4abac term=none ctermbg=0 ctermfg=15

" Constructs
" ----------
hi Constant guifg=#4ab2d7 ctermfg=4
hi Number guifg=#4ab2d7 ctermfg=4
hi Float guifg=#4ab2d7 ctermfg=4
hi String guifg=#8ba7ea ctermfg=12

hi Function guifg=#e58f84 ctermfg=5
hi Identifier guifg=#e08bd6 term=none ctermfg=13
hi Label guifg=#e58f84 ctermfg=5
hi Tag guifg=#e58f84 ctermfg=5
hi Keyword gui=bold guifg=#e08bd6 gui=bold ctermfg=13

hi Character gui=bold guifg=#2cbab6 cterm=bold ctermfg=14

hi Type gui=none,bold guifg=#2ab7bb term=none cterm=none,bold ctermfg=6
hi Boolean guifg=#2ab7bb ctermfg=6
hi StorageClass guifg=#2ab7bb ctermfg=6
hi Structure guifg=#2ab7bb ctermfg=6
hi Typedef gui=bold guifg=#2cbab6 cterm=bold ctermfg=14

hi Conditional gui=bold guifg=#6aba39 cterm=bold ctermfg=2
hi Statement gui=none guifg=#60ba80 cterm=none ctermfg=10
hi Repeat gui=bold guifg=#60ba80 cterm=bold ctermfg=10
hi Operator gui=bold guifg=#b4abac cterm=bold ctermfg=15
hi Exception gui=bold guifg=#ff7e8f cterm=bold ctermfg=1

hi Preproc gui=none guifg=#f78e2f term=none cterm=none ctermfg=9
hi PreCondit gui=bold guifg=#f78e2f cterm=bold ctermfg=9
hi Macro gui=bold guifg=#f78e2f cterm=bold ctermfg=9
hi Include guifg=#f78e2f ctermfg=9
hi Define guifg=#f78e2f ctermfg=9

hi Title gui=bold guibg=#090a18 guifg=#2ab7bb cterm=bold ctermbg=0 ctermfg=6

hi Delimeter gui=bold guifg=#e58f84 cterm=bold ctermfg=5
hi Delimiter gui=bold guifg=#e58f84 cterm=bold ctermfg=5
hi SpecialComment gui=bold guifg=#e58f84 cterm=bold ctermfg=5

hi Debug guifg=#e08bd6 ctermfg=13

" Other
" -----
hi LineNr guibg=#2b1329 guifg=#a7a2c4 term=none ctermbg=8 ctermfg=7
hi Cursor guibg=#b4abac guifg=#090a18 ctermbg=15 ctermfg=0
hi CursorLine gui=none guibg=#2b1329 term=none cterm=none ctermbg=8
hi CursorColumn gui=none guibg=#2b1329 term=none cterm=none ctermbg=8
hi CursorLineNr gui=bold guibg=#a7a2c4 guifg=#090a18 cterm=bold ctermbg=7 ctermfg=0
hi ColorColumn guibg=#2b1329 guifg=#b4abac term=none ctermbg=8 ctermfg=15
hi SignColumn guibg=#2b1329 guifg=#a7a2c4 term=none ctermbg=8 ctermfg=7

hi Folded guibg=#2b1329 guifg=#a7a2c4 ctermbg=8 ctermfg=7
hi FoldColumn guibg=#2b1329 guifg=#a7a2c4 ctermbg=8 ctermfg=7

hi Special gui=bold guifg=#de9b1d term=none cterm=bold ctermfg=11
hi SpecialKey gui=none guibg=#2b1329 guifg=#a7a2c4 cterm=none ctermbg=8 ctermfg=7
hi SpecialChar gui=bold guifg=#de9b1d cterm=bold ctermfg=11
hi NonText gui=none guibg=#2b1329 guifg=#a7a2c4 cterm=none ctermbg=8 ctermfg=7
hi EndOfBuffer gui=bold guifg=#a7a2c4 cterm=bold ctermfg=7

hi Directory gui=none guifg=#6aba39 term=none cterm=none ctermfg=2
hi Question gui=bold guifg=#de9b1d cterm=bold ctermfg=11
hi MoreMsg guifg=#60ba80 ctermfg=10
hi ModeMsg gui=bold guifg=#6aba39 cterm=bold ctermfg=2

hi VimOption guifg=#e58f84 ctermfg=5
hi VimGroup guifg=#e58f84 ctermfg=5

hi Underlined gui=underline guifg=#b4abac cterm=underline ctermfg=15
hi Ignore guibg=#2b1329 guifg=#a7a2c4 ctermbg=8 ctermfg=7
hi Conceal guibg=#a7a2c4 guifg=#2b1329 ctermbg=7 ctermfg=8

hi SpellBad guibg=#ff7e8f guifg=#090a18 ctermbg=1 ctermfg=0
hi SpellCap guibg=#bfa51a guifg=#090a18 ctermbg=3 ctermfg=0
hi SpellRare guibg=#e08bd6 guifg=#090a18 ctermbg=13 ctermfg=0
hi SpellLocal guibg=#2cbab6 guifg=#090a18 ctermbg=14 ctermfg=0

hi Pmenu gui=italic guibg=#2b1329 guifg=#b4abac cterm=none ctermbg=8 ctermfg=15
hi PmenuSel gui=none,bold guibg=#a7a2c4 guifg=#090a18 cterm=none,bold ctermbg=7 ctermfg=0
hi PmenuSbar guibg=#2b1329 ctermbg=8
hi PmenuThumb guibg=#a7a2c4 ctermbg=7

" Terminal
" --------

if exists('*term_setansicolors')
	let g:terminal_ansi_colors = [
				\ "#090a18",
				\ "#ff7e8f",
				\ "#6aba39",
				\ "#bfa51a",
				\ "#4ab2d7",
				\ "#e58f84",
				\ "#2ab7bb",
				\ "#a7a2c4",
				\ "#2b1329",
				\ "#f78e2f",
				\ "#60ba80",
				\ "#de9b1d",
				\ "#8ba7ea",
				\ "#e08bd6",
				\ "#2cbab6",
				\ "#b4abac"
				\ ]
endif
if has('nvim')
	let g:terminal_color_0 = "#090a18"
	let g:terminal_color_1 = "#ff7e8f"
	let g:terminal_color_2 = "#6aba39"
	let g:terminal_color_3 = "#bfa51a"
	let g:terminal_color_4 = "#4ab2d7"
	let g:terminal_color_5 = "#e58f84"
	let g:terminal_color_6 = "#2ab7bb"
	let g:terminal_color_7 = "#a7a2c4"
	let g:terminal_color_8 = "#2b1329"
	let g:terminal_color_9 = "#f78e2f"
	let g:terminal_color_10 = "#60ba80"
	let g:terminal_color_11 = "#de9b1d"
	let g:terminal_color_12 = "#8ba7ea"
	let g:terminal_color_13 = "#e08bd6"
	let g:terminal_color_14 = "#2cbab6"
	let g:terminal_color_15 = "#b4abac"
endif

" Diffs
" -----
hi DiffAdd gui=bold guibg=#6aba39 guifg=#090a18 cterm=bold ctermbg=10 ctermfg=0
hi DiffDelete gui=none guibg=#ff7e8f guifg=#090a18 cterm=none ctermbg=9 ctermfg=0
hi DiffChange gui=bold guibg=#2b1329 guifg=#a7a2c4 cterm=bold ctermbg=8 ctermfg=7
hi DiffText gui=bold guibg=#2b1329 guifg=#f78e2f cterm=bold ctermbg=8 ctermfg=1

hi diffAdded guifg=#6aba39 ctermfg=2
hi diffRemoved guifg=#ff7e8f ctermfg=1
hi diffNewFile gui=none guifg=#4ab2d7 ctermfg=4
hi diffFile gui=none guifg=#bfa51a cterm=none ctermfg=3

hi GitGutterAdd guibg=#2b1329 guifg=#6aba39 ctermbg=8 ctermfg=2
hi GitGutterChange gui=bold guibg=#2b1329 guifg=#a7a2c4 cterm=bold ctermbg=8 ctermfg=7
hi GitGutterDelete guibg=#2b1329 guifg=#ff7e8f ctermbg=8 ctermfg=1
hi GitGutterChangeDelete gui=bold guibg=#2b1329 guifg=#ff7e8f cterm=bold ctermbg=8 ctermfg=1

" Neomake
" -------
hi NeomakeError gui=none guibg=#ff7e8f guifg=#090a18 cterm=none ctermbg=1 ctermfg=0
hi NeomakeInfo gui=none guibg=#2ab7bb guifg=#090a18 cterm=none ctermbg=6 ctermfg=0
hi NeomakeWarning gui=none guibg=#bfa51a guifg=#090a18 cterm=none ctermbg=3 ctermfg=0
hi NeomakeMessage gui=none guibg=#de9b1d guifg=#090a18 cterm=none ctermbg=11 ctermfg=0

hi NeomakeVirtualtextInfoDefault guifg=#2cbab6 ctermfg=14
hi NeomakeVirtualtextMessageDefault guifg=#de9b1d ctermfg=11
hi NeomakeVirtualtextWarningDefault guifg=#bfa51a ctermfg=3
hi NeomakeVirtualtextErrorDefault guifg=#ff7e8f ctermfg=1

hi NeomakeStatusGood gui=none guibg=#6aba39 guifg=#090a18 cterm=none ctermbg=2 ctermfg=0
hi NeomakeStatusGoodNC gui=none guibg=#2b1329 guifg=#6aba39 cterm=none ctermbg=8 ctermfg=2

hi NeomakeStatColorDefault gui=none guibg=#4ab2d7 guifg=#090a18 cterm=none ctermbg=4 ctermfg=0
hi NeomakeStatColorTypeE gui=none guibg=#ff7e8f guifg=#090a18 cterm=none ctermbg=1 ctermfg=0
hi NeomakeStatColorTypeW gui=none guibg=#bfa51a guifg=#090a18 cterm=none ctermbg=3 ctermfg=0

" Markdown
" --------
hi MarkdownRule gui=bold guibg=#2b1329 guifg=#60ba80 cterm=bold ctermbg=8 ctermfg=10

hi MarkdownHeading gui=bold guifg=#b4abac cterm=bold ctermfg=15
hi default link MarkdownH1 MarkdownHeading
hi default link MarkdownH2 MarkdownHeading
hi default link MarkdownH3 MarkdownHeading
hi default link MarkdownH4 MarkdownHeading
hi default link MarkdownH5 MarkdownHeading
hi default link MarkdownH6 MarkdownHeading
hi default link MarkdownHeadingDelimiter MarkdownHeading
hi default link MarkdownHeadingRule MarkdownHeading

hi MarkdownBold gui=bold guifg=#f78e2f cterm=bold ctermfg=9
hi default link MarkdownBoldDelimiter MarkdownBold

hi MarkdownItalic gui=italic guifg=#bfa51a cterm=none ctermfg=3
hi default link MarkdownItalicDelimiter MarkdownItalic

hi MarkdownUrl gui=underline guifg=#4ab2d7 cterm=underline ctermfg=4
hi MarkdownLinkText gui=none guifg=#8ba7ea cterm=none ctermfg=12
hi MarkdownLinkDelimiter gui=bold guifg=#b4abac cterm=bold ctermfg=15
hi default link MarkdownLinkTextDelimiter MarkdownLinkDelimiter

hi MarkdownCode guifg=#e58f84 ctermfg=5
hi default link MarkdownCodeDelimiter MarkdownCode

hi MarkdownCodeBlock guifg=#b4abac ctermfg=15

hi MarkdownListMarker gui=none guifg=#6aba39 cterm=none ctermfg=2
hi default link MarkdownOrderedListMarker MarkdownListMarker

" Linting
" -------
hi YcmErrorSection gui=undercurl guisp=#ff7e8f cterm=underline
hi YcmWarningSection gui=undercurl guisp=#bfa51a cterm=underline
hi SyntasticError gui=undercurl guisp=#ff7e8f cterm=underline
hi SyntasticWarning gui=undercurl guisp=#bfa51a cterm=underline
hi SyntasticErrorSing guifg=#090a18 guibg=#ff7e8f ctermfg=0 ctermbg=1
hi SyntasticWarningSign guifg=#090a18 guibg=#bfa51a ctermfg=0 ctermbg=3
