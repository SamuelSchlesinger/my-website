syntax on
filetype plugin indent on

set nocompatible
set path+=**
set showcmd
set wildmenu
set expandtab
set tabstop=2
set softtabstop=2
set shiftwidth=2
set nofoldenable
set number
set relativenumber
set lazyredraw
set showmatch
set laststatus=2
set ruler
let g:netrw_banner=0
let g:netrw_browse_split=4
let g:netrw_altv=1
let g:netrw_liststyle=3
let g:netrw_list_hide=netrw_gitignore#Hide()
let g:netrw_list_hide.=',\(^|\s\s\)\zs\.\S\+'

command! MakeTags !hasktags -c -R .

nnoremap ,l gg0{-# LANGUAGE  #-}<esc>3hi

nnoremap ,c !stack ghci %
