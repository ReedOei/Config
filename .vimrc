set nocompatible              " be iMproved, required
filetype off                  " required

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
" let Vundle manage Vundle, required
Plugin 'VundleVim/Vundle.vim'
Plugin 'joom/latex-unicoder.vim'
Plugin 'scrooloose/nerdtree'
Plugin 'tpope/vim-surround'
Plugin 'scrooloose/nerdcommenter'
Plugin 'easymotion/vim-easymotion'
Plugin 'bling/vim-airline'
Plugin 'ervandew/supertab'
Plugin 'Shougo/neocomplete.vim'
Plugin 'JuliaEditorSupport/julia-vim'
Plugin 'idris-hackers/idris-vim'
Plugin 'vim-syntastic/syntastic'
Plugin 'eagletmt/ghcmod-vim'
Plugin 'eagletmt/neco-ghc'
Plugin 'Shougo/vimproc.vim'
Plugin 'udalov/kotlin-vim'
Plugin 'junegunn/vim-easy-align'
Plugin 'tpope/vim-commentary'
Plugin 'rust-lang/rust.vim'
Plugin 'airblade/vim-gitgutter'
Plugin 'vim-scripts/vim-svngutter'
Plugin 'cloudhead/neovim-ghcid'

" All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin indent on    " required

autocmd FileType idris setlocal commentstring=--\ %s

:set autoindent
:set hlsearch

:set completeopt=longest,menuone
let g:neocomplete#enable_at_startup = 1

set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

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

map <Leader> <Plug>(easymotion-prefix)

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

augroup Binary
    au!
    au BufReadPre  *.ser let &bin=1
    au BufReadPost *.ser if &bin | %!xxd
    au BufReadPost *.ser set ft=xxd | endif
    au BufWritePre *.ser if &bin | %!xxd -r
    au BufWritePre *.ser endif
    au BufWritePost *.ser if &bin | %!xxd
    au BufWritePost *.ser set nomod | endif
augroup END

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

autocmd FileType cpp nnoremap <F2> :call MakeHeader()<CR>
autocmd FileType cpp nnoremap <F3> :call WriteClassImpl()<CR>

" set autoread
" au FocusGained,BufEnter * :checktime " Check for autoread.

