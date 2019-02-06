" Name: Tempus Past
" Author: Protesilaos Stavrou (https://protesilaos.com)
" Description: Light theme inspired by old vaporwave concept art (WCAG AA compliant)

set background=light
hi clear
if exists("syntax_on")
  syntax reset
endif
let g:colors_name = "tempus_past"

" General
" -----------------

" NOTE the ctermbg=none is for terminals with transparency
hi Normal guibg=#F3F2F4 guifg=#53545B ctermbg=none ctermfg=0
hi Visual guibg=#53545B guifg=#F3F2F4 ctermbg=0 ctermfg=15
hi Search gui=underline,bold,italic guibg=#0A7040 guifg=#F3F2F4 cterm=underline,bold,italic ctermbg=2 ctermfg=15
hi IncSearch gui=underline,bold,italic guibg=#80565d guifg=#F3F2F4 term=none cterm=underline,bold,italic ctermbg=8 ctermfg=15

hi StatusLine gui=none,bold guibg=#53545B guifg=#F3F2F4 cterm=none,bold ctermbg=0 ctermfg=15
hi StatusLineNC gui=none guibg=#ECE6DE guifg=#80565d cterm=none ctermbg=7 ctermfg=8
hi StatusLineTerm gui=none,bold guibg=#0A7040 guifg=#F3F2F4 cterm=none,bold ctermbg=2 ctermfg=15
hi StatusLineTermNC gui=none guibg=#ECE6DE guifg=#0A7040 cterm=none ctermbg=7 ctermfg=2

hi VertSplit gui=none cterm=none
hi TabLine gui=none guibg=#ECE6DE guifg=#80565d cterm=none ctermbg=7 ctermfg=8
hi TabLineSel gui=none guibg=#53545B guifg=#F3F2F4 cterm=none ctermbg=0 ctermfg=15
hi TabLineFill gui=none cterm=none

hi Comment gui=italic guifg=#80565d cterm=italic ctermfg=8
hi Todo gui=bold guibg=#ECE6DE guifg=#9D524A cterm=bold ctermbg=7 ctermfg=11

hi Warning gui=none guibg=#A6403A guifg=#F3F2F4 cterm=none ctermbg=3 ctermfg=15
hi WarningMsg gui=none guibg=#A6403A guifg=#F3F2F4 cterm=none ctermbg=3 ctermfg=15
hi Error gui=none guibg=#C00C50 guifg=#F3F2F4 cterm=none ctermbg=1 ctermfg=15
hi ErrorMsg gui=none guibg=#C00C50 guifg=#F3F2F4 cterm=none ctermbg=1 ctermfg=15

hi MatchParen gui=underline,bold guibg=#B225AB guifg=#F3F2F4 cterm=underline,bold ctermbg=13 ctermfg=15

hi ToolbarLine guibg=#80565d guifg=#F3F2F4 term=none ctermbg=8 ctermfg=15
hi ToolbarButton gui=bold guibg=#80565d guifg=#F3F2F4 term=none cterm=bold ctermbg=8 ctermfg=15

hi WildMenu guibg=#F3F2F4 guifg=#53545B term=standout ctermbg=15 ctermfg=0

hi Terminal guibg=#F3F2F4 guifg=#53545B term=none ctermbg=15 ctermfg=0

" Constructs
" -----------------
hi Constant guifg=#1763AA ctermfg=4
hi Number guifg=#1763AA ctermfg=4
hi Float guifg=#1763AA ctermfg=4
hi String guifg=#5A5EBB ctermfg=12

hi Function guifg=#B02874 ctermfg=5
hi Identifier guifg=#B225AB term=none ctermfg=13
hi Label guifg=#B02874 ctermfg=5
hi Tag guifg=#B02874 ctermfg=5
hi Keyword gui=bold guifg=#B225AB gui=bold ctermfg=13

hi Character gui=bold guifg=#07737A cterm=bold ctermfg=14

hi Type gui=none,bold guifg=#096A83 term=none cterm=none,bold ctermfg=6
hi Boolean guifg=#096A83 ctermfg=6
hi StorageClass guifg=#096A83 ctermfg=6
hi Structure guifg=#096A83 ctermfg=6
hi Typedef gui=bold guifg=#07737A cterm=bold ctermfg=14

hi Conditional gui=bold guifg=#0A7040 cterm=bold ctermfg=2
hi Statement gui=none guifg=#407343 cterm=none ctermfg=10
hi Repeat gui=bold guifg=#407343 cterm=bold ctermfg=10
hi Operator gui=bold guifg=#53545B cterm=bold ctermfg=0
hi Exception gui=bold guifg=#C00C50 cterm=bold ctermfg=1

hi Preproc gui=none guifg=#BD3636 term=none cterm=none ctermfg=9
hi PreCondit gui=bold guifg=#BD3636 cterm=bold ctermfg=9
hi Macro gui=bold guifg=#BD3636 cterm=bold ctermfg=9
hi Include guifg=#BD3636 ctermfg=9
hi Define guifg=#BD3636 ctermfg=9

hi Title gui=bold guibg=#F3F2F4 guifg=#096A83 cterm=bold ctermbg=15 ctermfg=6

hi Special gui=bold guifg=#9D524A term=none cterm=bold ctermfg=11
hi SpecialKey guifg=#9D524A ctermfg=11
hi SpecialChar gui=bold guifg=#9D524A cterm=bold ctermfg=11

hi Delimeter gui=bold guifg=#B02874 cterm=bold ctermfg=5
hi Delimiter gui=bold guifg=#B02874 cterm=bold ctermfg=5
hi SpecialComment gui=bold guifg=#B02874 cterm=bold ctermfg=5

hi Debug guifg=#B225AB ctermfg=13

" Other
" -----------------
hi LineNr guibg=#ECE6DE guifg=#80565d term=none ctermbg=7 ctermfg=8
hi Cursor guibg=#53545B guifg=#F3F2F4 ctermbg=0 ctermfg=15
hi CursorLine gui=none guibg=NONE term=none cterm=none ctermbg=none
hi CursorColumn gui=none guibg=#ECE6DE term=none cterm=none ctermbg=7
hi CursorLineNr gui=bold guibg=#80565d guifg=#F3F2F4 cterm=bold ctermbg=8 ctermfg=15
hi ColorColumn guibg=#ECE6DE guifg=#53545B term=none ctermbg=7 ctermfg=0

hi Folded guibg=#ECE6DE guifg=#80565d ctermbg=7 ctermfg=8
hi FoldColumn guibg=#ECE6DE guifg=#80565d ctermbg=7 ctermfg=8

hi NonText gui=bold guibg=NONE guifg=#80565d cterm=bold ctermbg=none ctermfg=8

hi Directory gui=none guifg=#0A7040 term=none cterm=none ctermfg=2
hi Question gui=bold guifg=#9D524A cterm=bold ctermfg=11
hi MoreMsg guifg=#407343 ctermfg=10
hi ModeMsg gui=bold guifg=#0A7040 cterm=bold ctermfg=2

hi VimOption guifg=#B02874 ctermfg=5
hi VimGroup guifg=#B02874 ctermfg=5

hi Underlined gui=underline,bold guifg=#53545B cterm=underline,bold ctermfg=0
hi Ignore guibg=#ECE6DE guifg=#80565d ctermbg=7 ctermfg=8
hi Conceal guibg=#80565d guifg=#ECE6DE ctermbg=8 ctermfg=7

hi SpellBad guibg=#C00C50 guifg=#F3F2F4 ctermbg=1 ctermfg=15
hi SpellCap guibg=#A6403A guifg=#F3F2F4 ctermbg=3 ctermfg=15
hi SpellRare guibg=#B225AB guifg=#F3F2F4 ctermbg=13 ctermfg=15
hi SpellLocal guibg=#07737A guifg=#F3F2F4 ctermbg=14 ctermfg=15

hi Pmenu gui=italic guibg=#ECE6DE guifg=#53545B cterm=italic ctermbg=7 ctermfg=0
hi PmenuSel gui=none,bold guibg=#80565d guifg=#F3F2F4 cterm=none,bold ctermbg=8 ctermfg=15
hi PmenuSbar guibg=#ECE6DE ctermbg=7
hi PmenuThumb guibg=#80565d ctermbg=8

" Diffs
" -----------------
hi DiffAdd gui=bold guibg=#0A7040 guifg=#F3F2F4 cterm=bold ctermbg=10 ctermfg=15
hi DiffDelete gui=none guibg=#C00C50 guifg=#F3F2F4 cterm=none ctermbg=9 ctermfg=15
hi DiffChange gui=bold guibg=#ECE6DE guifg=#80565d cterm=bold ctermbg=7 ctermfg=8
hi DiffText gui=bold guibg=#ECE6DE guifg=#BD3636 cterm=bold ctermbg=7 ctermfg=1

hi diffAdded guifg=#0A7040 ctermfg=2
hi diffRemoved guifg=#C00C50 ctermfg=1
hi diffNewFile gui=none guifg=#1763AA ctermfg=4
hi diffFile gui=none guifg=#A6403A cterm=none ctermfg=3
