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
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0

let g:syntastic_haskell_checkers = ['hlint']
let g:syntastic_idris_checkers = ['idris']

syntax on
set number
set tabstop=8 softtabstop=0 expandtab shiftwidth=4 smarttab

xmap ga <Plug>(EasyAlign)

map <Leader> <Plug>(easymotion-prefix)

" Strips all the trailing whitespace on save.
autocmd BufWritePre * %s/\s\+$//e

" Run GhcModCheck on write
" autocmd BufWritePost *.hs GhcModCheck

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

