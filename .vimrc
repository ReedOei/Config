set nocompatible              " be iMproved, required
filetype off                  " required

set t_Co=256

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
" let Vundle manage Vundle, required
Plugin 'VundleVim/Vundle.vim'
Plugin 'joom/latex-unicoder.vim'
" Plugin 'scrooloose/nerdcommenter'
" Plugin 'bling/vim-airline'
" Plugin 'ervandew/supertab'
" Plugin 'Shougo/neocomplete.vim'
" Plugin 'JuliaEditorSupport/julia-vim'
Plugin 'tpope/vim-surround'
Plugin 'tpope/vim-repeat'
Plugin 'idris-hackers/idris-vim'
Plugin 'vim-syntastic/syntastic'
Plugin 'eagletmt/neco-ghc'
Plugin 'Shougo/vimproc.vim'
Plugin 'junegunn/vim-easy-align'
Plugin 'tpope/vim-commentary'
Plugin 'rust-lang/rust.vim'
Plugin 'airblade/vim-gitgutter'
" Plugin 'lervag/vimtex'
" Plugin 'vim-scripts/mercury.vim'
Plugin 'adimit/prolog.vim'
" Plugin 'ARM9/mips-syntax-vim'
" Plugin 'lifepillar/vim-solarized8'
Plugin 'altercation/vim-colors-solarized'
Plugin 'derekwyatt/vim-scala'
Plugin 'wimstefan/vim-artesanal'
" Plugin 'brafales/vim-desert256'
" Plugin 'vim-scripts/Simple256'
Plugin 'derekelkins/agda-vim'
" Plugin 'chris-bacon/haskell-refactor'
" Plugin 'dan-t/vim-hsimport'
Plugin 'zxqfl/tabnine-vim'
Plugin 'ReedOei/iolia'
Plugin 'ReedOei/vim-maude'
Plugin 'ReedOei/vim-enki'
Plugin 'tomlion/vim-solidity'
Plugin 'atelierbram/vim-colors_atelier-schemes'
Plugin 'brandonbloom/vim-factor'
Plugin 'andy-morris/mizar.vim'

" All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin indent on    " required

" colorscheme iolia
" colorscheme artesanal
" colorscheme simple256
colorscheme solarized

let g:surround_{char2nr('c')} = "\\\1command\1{\r}"

autocmd BufRead,BufNewFile *.pl set filetype=prolog
" autocmd BufRead,BufNewFile *.m set filetype=mercury
au BufNewFile,BufRead *.s,*.S set filetype=mips

autocmd FileType idris setlocal commentstring=--\ %s
autocmd FileType agda setlocal commentstring=--\ %s
autocmd FileType maude setlocal commentstring=---\ %s

:set autoindent
:set hlsearch

:set completeopt=longest,menuone
let g:neocomplete#enable_at_startup = 1

set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()} " Remove thsi if syntastic is removed
set statusline+=%*

let g:necoghc_enable_detailed_browse = 1
let g:necoghc_use_stack = 1
let g:necoghc_debug = 1

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 0
let g:syntastic_check_on_wq = 0

" let g:syntastic_haskell_checkers = ['hlint', 'hdevtools']
let g:syntastic_idris_checkers = ['idris']
let g:syntastic_rust_checkers = ['cargo']
let g:syntastic_java_checkers = []

let g:syntastic_cpp_compiler = 'clang++'
let g:syntastic_cpp_compiler_options = ' -std=c++11 -stdlib=libc++'

let g:syntastic_disabled_filetypes = ['java', 'haskell']

syntax on
set number

set scrolloff=3

xmap ga <Plug>(EasyAlign)

" Strips all the trailing whitespace on save.
autocmd BufWritePre * %s/\s\+$//e

" Run GhcModCheck on write
" autocmd BufWritePost *.hs GhcModCheck

set expandtab smarttab tabstop=4 shiftwidth=4
if has("autocmd")
    " If the filetype is Makefile then we need to use tabs
    " So do not expand tabs into space.
    autocmd FileType make setlocal noexpandtab
endif

set tabpagemax=1000

function! MakeHeader()
    let name = input('Class name: ')
    normal ggO
    call setline('.', ["#ifndef " . toupper(name) . "_H", "#define " . toupper(name) . "_H", "", "class " . name . " {", "", "};", ""])

    normal Go
    call setline('.', ["#endif // " . toupper(name) . "_H", ""])
endfunction

function! WriteClassImpl()
    :silent !bash ~/Config/bin/write_class_impl %
    :redraw!

    echom "Wrote class implementation."
endfunction

function! MakeTurtleScript()
    normal ggO
    call append('.', ["#!/usr/bin/env stack", "{- stack", "   script", "   --resolver lts-12.16", "   --package turtle", "-}", ""])
    normal dd
endfunction

function! CheckHaskellScript()
    let line = getline(1)

    if matchend(line, "stack$") != -1
        set filetype=haskell
    endif
endfunction

function! WriteEnvInput()
    let name = input('Environment name: ')
    call WriteEnv(name)
endfunction

function! WriteEnv(name)
    call append('.', ["\\begin{" . a:name . "}", "", "\\end{" . a:name . "}", "\\begin{proof}", "", "\\end{proof}"])
    normal 2j
    startinsert
endfunction

function! WriteItemize()
    call append('.', ["\\begin{itemize}", "    \\item[]", "\\end{itemize}"])
    normal jj$
    startinsert
endfunction

function! PrintIterable()
    let name = input('Variable: ')

    call append('.', ["std::cout << \"" . name . ": [\";", "size_t idx_" . name . " = 0;", "for (auto s : " . name . ") {", "    std::cout << s;", "    if (idx_" . name . " < " . name . ".size() - 1) {", "        std::cout << \",\";", "    }", "    idx_" . name . "++;", "}", "std::cout << \"]\" << std::endl;"])
endfunction

" From: https://stackoverflow.com/a/6271254
function! GetVisualSelection()
    " Why is this not a built-in Vim script function?!
    let [line_start, column_start] = getpos("'<")[1:2]
    let [line_end, column_end] = getpos("'>")[1:2]
    let lines = getline(line_start, line_end)
    if len(lines) == 0
        return ''
    endif
    let lines[-1] = lines[-1][: column_end - (&selection == 'inclusive' ? 1 : 2)]
    let lines[0] = lines[0][column_start - 1:]
    return join(lines, "\n")
endfunction

function! PrePad(s,amt,...)
    if a:0 > 0
        let char = a:1
    else
        let char = ' '
    endif
    return repeat(char,a:amt - len(a:s)) . a:s
endfunction

function! Indent(str)
	return PrePad(a:str, len(a:str) + indent('.'))
endfunction

function! PrintVarAsD(visual)
    if a:visual
        let var = GetVisualSelection()
    else
        let var = expand("<cword>")
    endif

    let str = Indent("printf(\"" . var . " = %d\\n\", " . var . ");")

    normal k
    call append('.', [str])
endfunction

function! PrintVar(visual)
    if a:visual
        let var = GetVisualSelection()
    else
        let var = expand("<cword>")
    endif

    let t = input('Format as: ')
    let str = Indent("printf(\"" . var . " = %" . t . "\\n\", " . var . ");")

    normal k
    call append('.', [str])
endfunction

function! WritePrologVar()
    let var = expand("<cword>")

    let str = Indent("format('" . var . " = ~w~n', [" . var . "]),")

    call append('.', [str])
endfunction

autocmd BufReadPost * :call CheckHaskellScript()

autocmd FileType haskell nnoremap <F2> :call MakeTurtleScript()<CR>
autocmd FileType haskell nmap <silent> <F3> :silent update <bar> HsimportModule<CR>
autocmd FileType haskell nmap <silent> <F4> :silent update <bar> HsimportSymbol<CR>

autocmd FileType cpp nnoremap <F2> :call MakeHeader()<CR>
autocmd FileType cpp nnoremap <F3> :call WriteClassImpl()<CR>
autocmd FileType cpp nnoremap <F4> :call PrintIterable()<CR>

autocmd FileType c nnoremap <F2> :call PrintVarAsD(0)<CR>
autocmd FileType c nnoremap <F3> :call PrintVar(0)<CR>

autocmd FileType c vnoremap <F2> :call PrintVarAsD(1)<CR>
autocmd FileType c vnoremap <F3> :call PrintVar(1)<CR>

autocmd FileType tex nnoremap <F2> :call WriteEnv("proposition")<CR>
autocmd FileType tex nnoremap <F3> :call WriteEnvInput()<CR>
autocmd FileType tex nnoremap <F4> :call WriteItemize()<CR>

autocmd FileType prolog nnoremap <leader>w :call WritePrologVar()<CR>

nnoremap <F5> :!make<CR>

let g:haskell_enable_quantification = 1   " to enable highlighting of `forall`
let g:haskell_enable_recursivedo = 1      " to enable highlighting of `mdo` and `rec`
let g:haskell_enable_arrowsyntax = 1      " to enable highlighting of `proc`
let g:haskell_enable_pattern_synonyms = 1 " to enable highlighting of `pattern`
let g:haskell_enable_typeroles = 1        " to enable highlighting of type roles
let g:haskell_enable_static_pointers = 1  " to enable highlighting of `static`
let g:haskell_backpack = 1                " to enable highlighting of backpack keywords

function! CheckProlog()
    if getline(1) == "#!/usr/bin/env swipl"
        set filetype=prolog
    endif
endfunction

autocmd BufRead,BufNewFile * call CheckProlog()

" set autoread
" au FocusGained,BufEnter * :checktime " Check for autoread.

au BufRead,BufNewFile *.k set filetype=kframework
au! Syntax kframework source kframework.vim
syn on

au BufRead,BufNewFile *.maude set filetype=maude
au BufRead,BufNewFile *.enki set filetype=enki
au BufRead,BufNewFile *.envm set filetype=enkivm
au BufRead,BufNewFile *.obs set filetype=obsidian

set wildmode=longest,list

set backspace=indent,eol,start

function! MathHomeworkTemplate(input_str)
    let s = split(a:input_str)

    normal! o\begin{enumerate}

    for i in s
        normal! o\item

        let inner_prob_num = str2nr(i)

        if inner_prob_num > 0
            normal! o\begin{enumerate}
            for j in range(1, inner_prob_num)
                normal! o\item
                normal! o\begin{proof}
                normal! o\end{proof}
            endfor
            normal! o\end{enumerate}
        else
            normal! o\begin{proof}
            normal! o\end{proof}
        endif
        normal! o
    endfor

    " Delete the last empty line inserted by the above for loop
    normal! dd
    normal! o\end{enumerate}
endfunction

function! MathHomeworkTemplateInput()
    let prob_num = input('problem input list of subproblem count (e.g., "0 1 2 0 1"): ')
    call MathHomeworkTemplate(probs)
endfunction

