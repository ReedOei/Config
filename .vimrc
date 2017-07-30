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
Plugin 'jiangmiao/auto-pairs'
Plugin 'Shougo/neocomplete.vim'

" All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin indent on    " required

:set autoindent

:set completeopt=longest,menuone
let g:neocomplete#enable_at_startup = 1

syntax on
set number
set tabstop=8 softtabstop=0 expandtab shiftwidth=4 smarttab

map <C-t><up> :tabr<cr>
map <C-t><down> :tabl<cr>
map <C-t><left> :tabp<cr>
map <C-t><right> :tabn<cr>

map <Leader> <Plug>(easymotion-prefix)

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

