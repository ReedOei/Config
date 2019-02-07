" Name:         iolia
" Author:       ReedOei
" Maintainer:   ReedOei
" License:      MIT
"
" Adapted from https://github.com/jliu2179/vim-dark-meadow

highlight clear

if exists('syntax_on')
  syntax reset
endif

let g:colors_name = 'iolia'
set background=dark

" Text style
let s:italic    = 'italic'
let s:bold      = 'bold'
let s:underline = 'underline'
let s:none      = 'NONE'

" Helper function to set up highlight executions
function! s:highlight(group, fg, bg, style)
  exec  "highlight "  . a:group
    \ . " ctermfg="   . a:fg[0]
    \ . " ctermbg="   . a:bg[0]
    \ . " cterm="     . a:style
    \ . " guifg="     . a:fg[1]
    \ . " guibg="     . a:bg[1]
    \ . " gui="       . a:style
endfunction

" Color palette
let s:clear           = ['NONE', 'NONE'   ]
let s:SteelBlue1      = ['81',   '#5fd7ff']
let s:Red1            = ['196',  '#ff0000']
let s:Magenta1        = ['201',  '#ff00ff']
let s:HotPink         = ['206',  '#ff5fd7']
let s:MediumOrchid1   = ['207',  '#ff5faf']
let s:MistyRose1      = ['224',  '#ffd7ff']
let s:LightCoral      = ['210',  '#ff8787']
let s:Plum1           = ['219',  '#ffafff']
let s:Thistle1        = ['225',  '#ffd7ff']
let s:Tan             = ['180',  '#d7af87']
let s:SkyBlue2        = ['111',  '#87afff']
let s:LightSkyBlue3   = ['109',  '#87afaf']
let s:DarkSlateGray1  = ['123',  '#87ffff']
let s:Gray62          = ['247',  '#9e9e9e']
let s:Gray7           = ['233',  '#121212']
let s:Gray42          = ['242',  '#6c6c6c']
let s:DeepPink4       = ['125',  '#af005f']
let s:Orchid          = ['170',  '#d75fd7']
let s:Magenta3        = ['163',  '#d700af']
let s:LightPink1      = ['217',  '#ffafaf']
let s:Violet          = ['177',  '#d787ff']
let s:Cornsilk1       = ['230',  '#ffffd7']
let s:LightGoldenrod2 = ['221',  '#ffd75f']

" Syntax highlighting groups
call s:highlight('Comment',        s:Thistle1,        s:clear, s:italic    )
call s:highlight('Constant',       s:MediumOrchid1,   s:clear, s:bold      )
call s:highlight('String',         s:SteelBlue1,      s:clear, s:none      )
call s:highlight('Character',      s:MediumOrchid1,   s:clear, s:none      )
call s:highlight('Number',         s:MediumOrchid1,   s:clear, s:italic      )
call s:highlight('Boolean',        s:MediumOrchid1,   s:clear, s:bold      )
call s:highlight('Float',          s:MediumOrchid1,   s:clear, s:italic      )
call s:highlight('Identifier',     s:Plum1,           s:clear, s:bold      )
call s:highlight('Function',       s:Violet,          s:clear, s:italic      )
call s:highlight('Statement',      s:Violet,          s:clear, s:italic      )
call s:highlight('Conditional',    s:Violet,          s:clear, s:italic      )
call s:highlight('Repeat',         s:Violet,          s:clear, s:bold      )
call s:highlight('Label',          s:Violet,          s:clear, s:bold      )
call s:highlight('Operator',       s:SkyBlue2,        s:clear, s:none      )
call s:highlight('Keyword',        s:Violet,          s:clear, s:none      )
call s:highlight('Exception',      s:Thistle1,        s:clear, s:italic    )
call s:highlight('PreProc',        s:Thistle1,        s:clear, s:italic    )
call s:highlight('Include',        s:Thistle1,        s:clear, s:italic    )
call s:highlight('Define',         s:Thistle1,        s:clear, s:italic    )
call s:highlight('Macro',          s:Thistle1,        s:clear, s:italic    )
call s:highlight('PreCondit',      s:Thistle1,        s:clear, s:italic    )
call s:highlight('Type',           s:Orchid,          s:clear, s:bold      )
call s:highlight('StorageClass',   s:Orchid,          s:clear, s:bold      )
call s:highlight('Structure',      s:LightGoldenrod2, s:clear, s:bold      )
call s:highlight('Typedef',        s:LightGoldenrod2, s:clear, s:bold      )
call s:highlight('Special',        s:LightGoldenrod2, s:clear, s:none      )
call s:highlight('SpecialChar',    s:LightGoldenrod2, s:clear, s:none      )
call s:highlight('Delimiter',      s:LightGoldenrod2, s:clear, s:none      )
call s:highlight('SpecialComment', s:LightGoldenrod2, s:clear, s:none      )
call s:highlight('Debug',          s:LightGoldenrod2, s:clear, s:none      )
call s:highlight('Underlined',     s:LightGoldenrod2, s:clear, s:underline )
call s:highlight('Error',          s:Red1,            s:clear, s:underline )
call s:highlight('Todo',           s:Red1,            s:clear, s:none      )

" Interface highlighting
call s:highlight('Normal',       s:clear,      s:clear,     s:none      )
call s:highlight('Visual',       s:clear,      s:DeepPink4, s:none      )
call s:highlight('Cursor',       s:LightCoral, s:clear,     s:none      )
call s:highlight('LineNr',       s:SkyBlue2,   s:clear,     s:none      )
call s:highlight('CursorLineNr', s:LightCoral, s:clear,     s:italic    )

