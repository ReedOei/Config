set nocompatible              " be iMproved, required
filetype off                  " required

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
" let Vundle manage Vundle, required
Plugin 'VundleVim/Vundle.vim'
Plugin 'joom/latex-unicoder.vim'
Plugin 'scrooloose/nerdcommenter'
Plugin 'bling/vim-airline'
Plugin 'ervandew/supertab'
Plugin 'Shougo/neocomplete.vim'
Plugin 'JuliaEditorSupport/julia-vim'
Plugin 'idris-hackers/idris-vim'
Plugin 'vim-syntastic/syntastic'
Plugin 'eagletmt/neco-ghc'
Plugin 'Shougo/vimproc.vim'
Plugin 'junegunn/vim-easy-align'
Plugin 'tpope/vim-commentary'
Plugin 'rust-lang/rust.vim'
Plugin 'airblade/vim-gitgutter'
Plugin 'vim-scripts/vim-svngutter'
Plugin 'vim-scripts/Conque-GDB'
Plugin 'vim-scripts/mercury.vim'
Plugin 'adimit/prolog.vim'

" All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin indent on    " required

autocmd BufRead,BufNewFile *.pl set filetype=prolog

autocmd FileType idris setlocal commentstring=--\ %s

:set autoindent
:set hlsearch

:set completeopt=longest,menuone
let g:neocomplete#enable_at_startup = 1

set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:necoghc_enable_detailed_browse = 1

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 0
let g:syntastic_check_on_wq = 0

let g:syntastic_haskell_checkers = ['hlint']
let g:syntastic_idris_checkers = ['idris']
let g:syntastic_rust_checkers = ['cargo']
let g:syntastic_java_checkers = []

let g:syntastic_cpp_compiler = 'clang++'
let g:syntastic_cpp_compiler_options = ' -std=c++11 -stdlib=libc++'

let g:syntastic_disabled_filetypes = ['java']

syntax on
set number

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

set tabpagemax=100

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
    call append('.', ["#!/usr/bin/env stack", "{- stack", "   script", "   --resolver lts-10.5", "   --package turtle", "-}", ""])
    normal dd
endfunction

function! CheckHaskellScript()
    let line = getline(1)

    if matchend(line, "stack$") != -1
        set filetype=haskell
    endif
endfunction

autocmd BufReadPost * :call CheckHaskellScript()

autocmd FileType haskell nnoremap <F2> :call MakeTurtleScript()<CR>

autocmd FileType cpp nnoremap <F2> :call MakeHeader()<CR>
autocmd FileType cpp nnoremap <F3> :call WriteClassImpl()<CR>

let g:haskell_enable_quantification = 1   " to enable highlighting of `forall`
let g:haskell_enable_recursivedo = 1      " to enable highlighting of `mdo` and `rec`
let g:haskell_enable_arrowsyntax = 1      " to enable highlighting of `proc`
let g:haskell_enable_pattern_synonyms = 1 " to enable highlighting of `pattern`
let g:haskell_enable_typeroles = 1        " to enable highlighting of type roles
let g:haskell_enable_static_pointers = 1  " to enable highlighting of `static`
let g:haskell_backpack = 1                " to enable highlighting of backpack keywords

" set autoread
" au FocusGained,BufEnter * :checktime " Check for autoread.

