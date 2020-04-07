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
" NOTE the ctermbg=none is for terminals with transparency
hi Normal guibg=#302420 guifg=#a9a2a6 ctermbg=none ctermfg=15
hi Visual guibg=#a9a2a6 guifg=#302420 ctermbg=15 ctermfg=0
hi Search gui=underline,bold guibg=#312e2a guifg=#a9a2a6 cterm=underline,bold ctermbg=8 ctermfg=3
hi IncSearch gui=underline,bold guibg=#a5918a guifg=#302420 term=none cterm=underline,bold ctermbg=7 ctermfg=0

hi StatusLine gui=none guibg=#a9a2a6 guifg=#302420 cterm=none ctermbg=15 ctermfg=0
hi StatusLineNC gui=none guibg=#312e2a guifg=#a5918a cterm=none ctermbg=8 ctermfg=7
hi StatusLineTerm gui=none guibg=#80a100 guifg=#302420 cterm=none ctermbg=2 ctermfg=0
hi StatusLineTermNC gui=none guibg=#312e2a guifg=#80a100 cterm=none ctermbg=8 ctermfg=2

hi VertSplit gui=none cterm=none
hi TabLine gui=none guibg=#312e2a guifg=#a5918a cterm=none ctermbg=8 ctermfg=7
hi TabLineSel gui=none guibg=#52a485 guifg=#302420 cterm=none ctermbg=6 ctermfg=0
hi TabLineFill gui=none cterm=none

hi Comment gui=italic guifg=#a5918a cterm=none ctermfg=7
hi Todo gui=bold guibg=#312e2a guifg=#ba9000 cterm=bold ctermbg=8 ctermfg=11

hi Warning gui=none guibg=#ac9440 guifg=#302420 cterm=none ctermbg=3 ctermfg=0
hi WarningMsg gui=none guibg=#ac9440 guifg=#302420 cterm=none ctermbg=3 ctermfg=0
hi Error gui=none guibg=#f16c50 guifg=#302420 cterm=none ctermbg=1 ctermfg=0
hi ErrorMsg gui=none guibg=#f16c50 guifg=#302420 cterm=none ctermbg=1 ctermfg=0

hi MatchParen gui=underline,bold guibg=#312e2a guifg=#a5918a cterm=underline,bold ctermbg=8 ctermfg=7

hi ToolbarLine guibg=#a5918a guifg=#302420 term=none ctermbg=7 ctermfg=0
hi ToolbarButton gui=bold guibg=#a5918a guifg=#302420 term=none cterm=bold ctermbg=7 ctermfg=0

hi WildMenu guibg=#302420 guifg=#a9a2a6 term=standout ctermbg=0 ctermfg=15

hi Terminal guibg=#302420 guifg=#a9a2a6 term=none ctermbg=0 ctermfg=15

" Constructs
" ----------
hi Constant guifg=#7897c2 ctermfg=4
hi Number guifg=#7897c2 ctermfg=4
hi Float guifg=#7897c2 ctermfg=4
hi String guifg=#908ed4 ctermfg=12

hi Function guifg=#dd758e ctermfg=5
hi Identifier guifg=#c57bc4 term=none ctermfg=13
hi Label guifg=#dd758e ctermfg=5
hi Tag guifg=#dd758e ctermfg=5
hi Keyword gui=bold guifg=#c57bc4 gui=bold ctermfg=13

hi Character gui=bold guifg=#2aa4ad cterm=bold ctermfg=14

hi Type gui=none,bold guifg=#52a485 term=none cterm=none,bold ctermfg=6
hi Boolean guifg=#52a485 ctermfg=6
hi StorageClass guifg=#52a485 ctermfg=6
hi Structure guifg=#52a485 ctermfg=6
hi Typedef gui=bold guifg=#2aa4ad cterm=bold ctermfg=14

hi Conditional gui=bold guifg=#80a100 cterm=bold ctermfg=2
hi Statement gui=none guifg=#43a770 cterm=none ctermfg=10
hi Repeat gui=bold guifg=#43a770 cterm=bold ctermfg=10
hi Operator gui=bold guifg=#a9a2a6 cterm=bold ctermfg=15
hi Exception gui=bold guifg=#f16c50 cterm=bold ctermfg=1

hi Preproc gui=none guifg=#e07a3d term=none cterm=none ctermfg=9
hi PreCondit gui=bold guifg=#e07a3d cterm=bold ctermfg=9
hi Macro gui=bold guifg=#e07a3d cterm=bold ctermfg=9
hi Include guifg=#e07a3d ctermfg=9
hi Define guifg=#e07a3d ctermfg=9

hi Title gui=bold guibg=#302420 guifg=#52a485 cterm=bold ctermbg=0 ctermfg=6

hi Delimeter gui=bold guifg=#dd758e cterm=bold ctermfg=5
hi Delimiter gui=bold guifg=#dd758e cterm=bold ctermfg=5
hi SpecialComment gui=bold guifg=#dd758e cterm=bold ctermfg=5

hi Debug guifg=#c57bc4 ctermfg=13

" Other
" -----
hi LineNr guibg=#312e2a guifg=#a5918a term=none ctermbg=8 ctermfg=7
hi Cursor guibg=#a9a2a6 guifg=#302420 ctermbg=15 ctermfg=0
hi CursorLine gui=none guibg=#312e2a term=none cterm=none ctermbg=8
hi CursorColumn gui=none guibg=#312e2a term=none cterm=none ctermbg=8
hi CursorLineNr gui=bold guibg=#a5918a guifg=#302420 cterm=bold ctermbg=7 ctermfg=0
hi ColorColumn guibg=#312e2a guifg=#a9a2a6 term=none ctermbg=8 ctermfg=15
hi SignColumn guibg=#312e2a guifg=#a5918a term=none ctermbg=8 ctermfg=7

hi Folded guibg=#312e2a guifg=#a5918a ctermbg=8 ctermfg=7
hi FoldColumn guibg=#312e2a guifg=#a5918a ctermbg=8 ctermfg=7

hi Special gui=bold guifg=#ba9000 term=none cterm=bold ctermfg=11
hi SpecialKey gui=none guibg=#312e2a guifg=#a5918a cterm=none ctermbg=8 ctermfg=7
hi SpecialChar gui=bold guifg=#ba9000 cterm=bold ctermfg=11
hi NonText gui=none guibg=#312e2a guifg=#a5918a cterm=none ctermbg=8 ctermfg=7
hi EndOfBuffer gui=bold guifg=#a5918a cterm=bold ctermfg=7

hi Directory gui=none guifg=#80a100 term=none cterm=none ctermfg=2
hi Question gui=bold guifg=#ba9000 cterm=bold ctermfg=11
hi MoreMsg guifg=#43a770 ctermfg=10
hi ModeMsg gui=bold guifg=#80a100 cterm=bold ctermfg=2

hi VimOption guifg=#dd758e ctermfg=5
hi VimGroup guifg=#dd758e ctermfg=5

hi Underlined gui=underline guifg=#a9a2a6 cterm=underline ctermfg=15
hi Ignore guibg=#312e2a guifg=#a5918a ctermbg=8 ctermfg=7
hi Conceal guibg=#a5918a guifg=#312e2a ctermbg=7 ctermfg=8

hi SpellBad guibg=#f16c50 guifg=#302420 ctermbg=1 ctermfg=0
hi SpellCap guibg=#ac9440 guifg=#302420 ctermbg=3 ctermfg=0
hi SpellRare guibg=#c57bc4 guifg=#302420 ctermbg=13 ctermfg=0
hi SpellLocal guibg=#2aa4ad guifg=#302420 ctermbg=14 ctermfg=0

hi Pmenu gui=italic guibg=#312e2a guifg=#a9a2a6 cterm=none ctermbg=8 ctermfg=15
hi PmenuSel gui=none,bold guibg=#a5918a guifg=#302420 cterm=none,bold ctermbg=7 ctermfg=0
hi PmenuSbar guibg=#312e2a ctermbg=8
hi PmenuThumb guibg=#a5918a ctermbg=7

" Diffs
" -----
hi DiffAdd gui=bold guibg=#80a100 guifg=#302420 cterm=bold ctermbg=10 ctermfg=0
hi DiffDelete gui=none guibg=#f16c50 guifg=#302420 cterm=none ctermbg=9 ctermfg=0
hi DiffChange gui=bold guibg=#312e2a guifg=#a5918a cterm=bold ctermbg=8 ctermfg=7
hi DiffText gui=bold guibg=#312e2a guifg=#e07a3d cterm=bold ctermbg=8 ctermfg=1

hi diffAdded guifg=#80a100 ctermfg=2
hi diffRemoved guifg=#f16c50 ctermfg=1
hi diffNewFile gui=none guifg=#7897c2 ctermfg=4
hi diffFile gui=none guifg=#ac9440 cterm=none ctermfg=3

hi GitGutterAdd guibg=#312e2a guifg=#80a100 ctermbg=8 ctermfg=2
hi GitGutterChange gui=bold guibg=#312e2a guifg=#a5918a cterm=bold ctermbg=8 ctermfg=7
hi GitGutterDelete guibg=#312e2a guifg=#f16c50 ctermbg=8 ctermfg=1
hi GitGutterChangeDelete gui=bold guibg=#312e2a guifg=#f16c50 cterm=bold ctermbg=8 ctermfg=1

" Neomake
" -------
hi NeomakeError gui=none guibg=#f16c50 guifg=#302420 cterm=none ctermbg=1 ctermfg=0
hi NeomakeInfo gui=none guibg=#52a485 guifg=#302420 cterm=none ctermbg=6 ctermfg=0
hi NeomakeWarning gui=none guibg=#ac9440 guifg=#302420 cterm=none ctermbg=3 ctermfg=0
hi NeomakeMessage gui=none guibg=#ba9000 guifg=#302420 cterm=none ctermbg=11 ctermfg=0

hi NeomakeVirtualtextInfoDefault guifg=#2aa4ad ctermfg=14
hi NeomakeVirtualtextMessageDefault guifg=#ba9000 ctermfg=11
hi NeomakeVirtualtextWarningDefault guifg=#ac9440 ctermfg=3
hi NeomakeVirtualtextErrorDefault guifg=#f16c50 ctermfg=1

hi NeomakeStatusGood gui=none guibg=#80a100 guifg=#302420 cterm=none ctermbg=2 ctermfg=0
hi NeomakeStatusGoodNC gui=none guibg=#312e2a guifg=#80a100 cterm=none ctermbg=8 ctermfg=2

hi NeomakeStatColorDefault gui=none guibg=#7897c2 guifg=#302420 cterm=none ctermbg=4 ctermfg=0
hi NeomakeStatColorTypeE gui=none guibg=#f16c50 guifg=#302420 cterm=none ctermbg=1 ctermfg=0
hi NeomakeStatColorTypeW gui=none guibg=#ac9440 guifg=#302420 cterm=none ctermbg=3 ctermfg=0

" Markdown
" --------
hi MarkdownRule gui=bold guibg=#312e2a guifg=#43a770 cterm=bold ctermbg=8 ctermfg=10

hi MarkdownHeading gui=bold guifg=#a9a2a6 cterm=bold ctermfg=15
hi default link MarkdownH1 MarkdownHeading
hi default link MarkdownH2 MarkdownHeading
hi default link MarkdownH3 MarkdownHeading
hi default link MarkdownH4 MarkdownHeading
hi default link MarkdownH5 MarkdownHeading
hi default link MarkdownH6 MarkdownHeading
hi default link MarkdownHeadingDelimiter MarkdownHeading
hi default link MarkdownHeadingRule MarkdownHeading

hi MarkdownBold gui=bold guifg=#e07a3d cterm=bold ctermfg=9
hi default link MarkdownBoldDelimiter MarkdownBold

hi MarkdownItalic gui=italic guifg=#ac9440 cterm=none ctermfg=3
hi default link MarkdownItalicDelimiter MarkdownItalic

hi MarkdownUrl gui=underline guifg=#7897c2 cterm=underline ctermfg=4
hi MarkdownLinkText gui=none guifg=#908ed4 cterm=none ctermfg=12
hi MarkdownLinkDelimiter gui=bold guifg=#a9a2a6 cterm=bold ctermfg=15
hi default link MarkdownLinkTextDelimiter MarkdownLinkDelimiter

hi MarkdownCode guifg=#dd758e ctermfg=5
hi default link MarkdownCodeDelimiter MarkdownCode

hi MarkdownCodeBlock guifg=#a9a2a6 ctermfg=15

hi MarkdownListMarker gui=none guifg=#80a100 cterm=none ctermfg=2
hi default link MarkdownOrderedListMarker MarkdownListMarker

" Linting
" -------
hi YcmErrorSection gui=undercurl guisp=#f16c50 cterm=underline
hi YcmWarningSection gui=undercurl guisp=#ac9440 cterm=underline
hi SyntasticError gui=undercurl guisp=#f16c50 cterm=underline
hi SyntasticWarning gui=undercurl guisp=#ac9440 cterm=underline
hi SyntasticErrorSing guifg=#302420 guibg=#f16c50 ctermfg=0 ctermbg=1
hi SyntasticWarningSign guifg=#302420 guibg=#ac9440 ctermfg=0 ctermbg=3
