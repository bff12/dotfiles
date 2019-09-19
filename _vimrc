"Update by Breno Farias 19.09.2019

:set encoding=utf-8
:set fileencoding=utf-8


colorscheme gruvbox
syntax on

execute pathogen#infect()
filetype plugin indent on

if has('gui_running')
 set guifont=IBM_Plex_Mono:h8
endif


set tabstop=2 "set size of tab 

set nobackup       "no backup files
set nowritebackup  "only in case you don't want a backup file while editing
set noswapfile     "no swap files
set noundofile    "no ~u files"
set belloff=all "disable sound
set guitablabel=[%N]%f  "show tab numbers

let g:ycm_autoclose_preview_window_after_insertion = 1 "no split buffer in autocomplete
let NERDTreeShowHidden=1 "make nerdtree show hidden files
let g:prettier#autoformat = 0
autocmd BufWritePre *.js,*.json,*.css,*.scss,*.less,*.graphql,*.ts,*.tsx,*.jsx Prettier "enable prettier on save with extensions
nnoremap <leader>b :buffers<cr>:b<space>  
"bind \b to show buffers 
nnoremap <leader>n :NERDTree<ENTER> 
"bind \n to show NERDTree 
nnoremap <F12> :bd<ENTER> 
"bind F12 to delete current buffer 
nnoremap <leader>va ggVG 
"bind \va to select all 
nnoremap mm $%V% 
"bind select matchs 
let NERDTreeQuitOnOpen = 0 "dont close NERDTree when open new buffer
let g:NERDTreeWinPos = "right" "set NERDTree to open in right
nmap <leader>1 <Plug>BuffetSwitch(1)
nmap <leader>2 <Plug>BuffetSwitch(2)
nmap <leader>3 <Plug>BuffetSwitch(3)
nmap <leader>4 <Plug>BuffetSwitch(4)
nmap <leader>5 <Plug>BuffetSwitch(5)
nmap <leader>6 <Plug>BuffetSwitch(6)
nmap <leader>7 <Plug>BuffetSwitch(7)
nmap <leader>8 <Plug>BuffetSwitch(8)
nmap <leader>9 <Plug>BuffetSwitch(9)
nmap <leader>0 <Plug>BuffetSwitch(10)
"vim-buffed binds
noremap <Tab> :bn<CR>
noremap <S-Tab> :bp<CR>
"navega entre buffers/tabs
noremap <C-t> :tabnew split<CR>
"cria tab para anotacoes? :)
let g:buffet_show_index = 1 "show index of buffer
nnoremap \d :bp<cr>:bd #<cr> 
"close buffer but not the window
