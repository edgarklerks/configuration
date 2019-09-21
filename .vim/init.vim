" dein Scripts-----------------------------
if &compatible
  set nocompatible               " Be iMproved
endif

" Required:
set runtimepath+=/home/eklerks/.cache/dein/repos/github.com/Shougo/dein.vim

" Required:
if dein#load_state('/home/eklerks/.cache/dein')
  call dein#begin('/home/eklerks/.cache/dein')

  " Let dein manage dein
  " Required:
  call dein#add('/home/eklerks/.cache/dein/repos/github.com/Shougo/dein.vim')

  " Add or remove your plugins here like this:
call dein#add('dense-analysis/ale')
call dein#add('Shougo/deoplete.nvim')
call dein#add('kien/ctrlp.vim')
  "call dein#add('Shougo/neosnippet.vim')
  "call dein#add('Shougo/neosnippet-snippets')

  " Required:
  call dein#end()
  call dein#save_state()
endif

" Required:
filetype plugin indent on
syntax enable

" If you want to install not installed plugins on startup.
if dein#check_install()
  call dein#install()
endif

"End dein Scripts-------------------------

let g:python3_host_prog='/home/eklerks/.vim/nvim-py/bin/python'

syntax on 
filetype plugin indent on 
set enc=utf-8
inoremap <expr><tab> pumvisible() ? "\<c-n>" : "\<tab>"
highlight ExtraWhitespace ctermbg=blue 
let g:ctrlp_root_markers = ['pom.xml', '.p4ignore','.git','build.sbt','.ctrlp_stop']
" Change vim directory 
set autochdir
set autoindent
" Map backspace 
set backspace=eol,indent,start
" Case, tab and indent. 
set smartcase 
set smartindent 
set smarttab 
" Search options (highlight and incremental)
set hlsearch
set incsearch
" Status amounts
set laststatus=2
set linespace=0
set listchars=tab:>-,trail:-
set matchtime=5
" Enable mouse for terminals
set mouse=a
" Disable annoying stuff 
set noerrorbells
set nostartofline
set novisualbell
" Minimal 5 columns for numbering 
set number
set numberwidth=5
" Relative numbering And ruler  
set relativenumber
set ruler
" Number of lines above us if we scroll
set scrolloff=10
" Abbreviate annoying messages 
set shortmess=aOstT
" Show some more 
set showcmd
set showmatch
set sidescrolloff=10
" Tab configuration
set shiftwidth=2 
set softtabstop=4
set tabstop=8
set expandtab
" Custom status line 
set statusline=%F%m%r%h%w[%L][%{&ff}]%y[%p%%][%04l,%04v]
" Activate command line completion
set wildmenu
set wildmode=list:longest
set wildoptions="df"
" Show line break
set showbreak ="+++ "
let $TERM="xterm-256color"
let $NVIM_TUI_ENABLE_TRUE_COLOR=0
set background=dark " or light
" colorscheme molokai
" colorscheme slate 
" colorscheme termschool
" colorscheme flattened_dark
let g:airline#extensions#tabline#enabled = 1
set statusline+=%#warningmsg#
set statusline+=%*
let g:deoplete#enable_at_startup = 1

let g:cloudformationModeSwitch = 0
let g:ansibleModeSwitch = 0

function AnsibleMode()
  if g:ansibleModeSwitch == 1
    echo "Switching ansible off"
    let g:ansibleModeSwitch = 0
  else 
    echo "Switching ansible on"
    let g:ansibleModeSwitch = 1
  endif
  let extension = expand('%:e')
  if g:ansibleModeSwitch == 0
    set syntax=yaml
    set filetype=yaml
  else 
    set filetype=ansible
    set syntax=yaml
  endif 
endfunction


function CloudformationMode()

  if g:cloudformationModeSwitch == 1
    echo "Switching cloudformation off"
    let g:cloudformationModeSwitch = 0
  else 
    let g:cloudformationModeSwitch = 1
    echo "Switching cloudformation on"
  endif 
let extension = expand('%:e')
if extension == "yaml"
    if g:cloudformationModeSwitch == 0
      set syntax=yaml
      set filetype=yaml
    else
      set filetype=cloudformation
      set syntax=yaml
    endif
endif
endfunction

