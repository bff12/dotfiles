"Update by Breno<brenomoreff@gmail.com> 06.08.2019
source $VIMRUNTIME/vimrc_example.vim

set diffexpr=MyDiff()
function MyDiff()
  let opt = '-a --binary '
  if &diffopt =~ 'icase' | let opt = opt . '-i ' | endif
  if &diffopt =~ 'iwhite' | let opt = opt . '-b ' | endif
  let arg1 = v:fname_in
  if arg1 =~ ' ' | let arg1 = '"' . arg1 . '"' | endif
  let arg1 = substitute(arg1, '!', '\!', 'g')
  let arg2 = v:fname_new
  if arg2 =~ ' ' | let arg2 = '"' . arg2 . '"' | endif
  let arg2 = substitute(arg2, '!', '\!', 'g')
  let arg3 = v:fname_out
  if arg3 =~ ' ' | let arg3 = '"' . arg3 . '"' | endif
  let arg3 = substitute(arg3, '!', '\!', 'g')
  if $VIMRUNTIME =~ ' '
    if &sh =~ '\<cmd'
      if empty(&shellxquote)
        let l:shxq_sav = ''
        set shellxquote&
      endif
      let cmd = '"' . $VIMRUNTIME . '\diff"'
    else
      let cmd = substitute($VIMRUNTIME, ' ', '" ', '') . '\diff"'
    endif
  else
    let cmd = $VIMRUNTIME . '\diff'
  endif
  let cmd = substitute(cmd, '!', '\!', 'g')
  silent execute '!' . cmd . ' ' . opt . arg1 . ' ' . arg2 . ' > ' . arg3
  if exists('l:shxq_sav')
    let &shellxquote=l:shxq_sav
  endif
endfunction::1
:set encoding=utf-8
:set fileencoding=utf-8

execute pathogen#infect()
syntax on
filetype plugin indent on

if has('gui_running')
 set guifont=IBM_Plex_Mono:h16
endif

colorscheme gruvbox
syntax on

set tabstop=2 "set size of tab 

set nobackup       "no backup files
set nowritebackup  "only in case you don't want a backup file while editing
set noswapfile     "no swap files
:set noundofile    "no ~u files"
let g:ycm_autoclose_preview_window_after_insertion = 1 "no split buffer in autocomplete
let NERDTreeShowHidden=1 "make nerdtree show hidden files
let g:prettier#autoformat = 0
autocmd BufWritePre *.js,*.json,*.css,*.scss,*.less,*.graphql,*.ts,*.tsx,*.jsx Prettier "enable prettier on save with extensions
set belloff=all "disable sound
set guitablabel=[%N]%f  "show tab numbers
nnoremap <leader>b :buffers<cr>:b<space>  
"bind \b to show buffers 
nnoremap <leader>n :NERDTree<ENTER> 
"bind \n to show NERDTree 
nnoremap <leader>db :bd<ENTER> 
"bind \db to delete current buffer 
nnoremap <leader>va ggVG 
"bind \va to select all 

