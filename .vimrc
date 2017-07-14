set encoding=utf-8

" Access colors present in 256 colorspace
let base16colorspace=256  
call plug#begin('~/.vim/plugged')


Plug 'tommcdo/vim-exchange'
Plug 'bronson/vim-visual-star-search'
Plug 'chriskempson/base16-vim'
Plug 'terryma/vim-expand-region'

call plug#end()

" Set encoding
set encoding=utf-8

" Searching
set hlsearch
set incsearch
set ignorecase
set smartcase

" Don't beep
set visualbell

" Personall Settings
set nocompatible
filetype plugin indent on
set smartindent
set autoindent
set number
set tabstop=4                   "A tab is 4 spaces
set expandtab                   "Always uses spaces instead of tabs
set softtabstop=4               "Insert 4 spaces when tab is pressed
set shiftwidth=4                "An indent is 4 spaces
set smarttab                    "Indent instead of tab at start of line
set shiftround                  "Round spaces to nearest shiftwidth multiple
set nojoinspaces
set wildmode=list:longest,full
set switchbuf=useopen
set guiheadroom=0
set guifontwide=Menlo:h11 

filetype plugin on

" Color theme settings
syntax enable
colorscheme base16-eighties
set background=dark

" Airline settings
let g:airline_powerline_fonts = 1
let g:airline_theme = 'base16'
" let g:airline#extensions#tabline#enabled = 1

set laststatus=2

" Alias for w! sudo tee %
cmap sw w !sudo tee > /dev/null %

" Show syntax highlighting groups for word under cursor
nmap <C-S-P> :call <SID>SynStack()<CR>
function! <SID>SynStack()
  if !exists("*synstack")
    return
  endif
  echo map(synstack(line('.'), col('.')), 'synIDattr(v:val, "name")')
endfunc

" Copying to clipboard
xmap <Esc>w "+y
vmap <Esc>w "+y
" vmap <M-w> :'<,'>yank +

" Bubble single lines
nmap <M-Up> [e
nmap <M-Down> ]e

" Bubble multiple lines
vmap <M-Down> ]egv
vmap <M-Up> [egv

" TextMate Indentation
map <M-[> <<
map <M-]> >>
vmap <M-]> >gv
vmap <M-[> <gv

" Save cursor position
augroup resCur
  autocmd!
  autocmd BufReadPost * call setpos(".", getpos("'\""))
augroup END
