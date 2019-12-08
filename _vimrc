"Updated by Breno<brenomoreff@gmail.com> 15.11.2019

source $VIMRUNTIME/vimrc_example.vim

set encoding=utf-8
set fileencoding=utf-8
set tabstop=2 "set size of tab 
set nobackup       "no backup files
set nowritebackup  "only in case you don't want a backup file while editing
set noswapfile     "no swap files
set noundofile    "no ~u files"
set belloff=all "disable sound
set guitablabel=[%N]%f  "show tab numbers
set hidden "allow move to other buffers without save

execute pathogen#infect()
syntax on
filetype plugin indent on

if has('gui_running')
 set guifont=IBM_Plex_Mono:h8
endif

colorscheme gruvbox
syntax on

autocmd BufWritePre *.js,*.json,*.css,*.scss,*.less,*.graphql,*.ts,*.tsx,*.jsx Prettier "enable prettier on save with extensions


let g:ycm_autoclose_preview_window_after_insertion = 1 "no split buffer in autocomplete
let NERDTreeShowHidden=1 "make nerdtree show hidden files
let g:prettier#autoformat = 0
let NERDTreeQuitOnOpen = 0 "dont close NERDTree when open new buffer
let g:NERDTreeWinPos = "right" "set NERDTree to open in right
let g:buffet_show_index = 1 "show index of buffer

"bind \b to show buffers 
nnoremap <leader>b :buffers<cr>:b<space>  
"bind \n to show NERDTree 
nnoremap <leader>n :NERDTree<ENTER> 
"bind F12 to delete current buffer 
nnoremap <F12> :bd<ENTER> 
"bind \va to select all 
nnoremap <leader>va ggVG 
"bind mm to select to end match
nnoremap mm $V% 
"vim-buffet binds
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
"navega entre buffers/tabs
noremap <Tab> :bn<CR>
noremap <S-Tab> :bp<CR>
"cria tab para anotacoes? :)
noremap <C-t> :tabnew split<CR>
"close buffer but not the window
nnoremap \d :bp<cr>:bd #<cr> 


""""""""Arquivos que podem ser adicionados de acordo com a necessidade 
"syntax match mySpecialSymbols "+\|-\|\*\|;\|?\|:\|,\|<\|>\|&\||\|!\|\~\|%\|=\|)\|(\|{\|}\|\.\|\[\|\]"
"hi mySpecialSymbols guifg=red
"autocmd VimEnter,BufNewFile,BufReadPost * silent! call HardMode() "disable movements keys
"autocmd BufEnter * colorscheme brenomore
"set number "show line number
"let g:NERDCustomDelimiters = { ''javascript': { 'left': '/***','right': '*********/' } } "custom comment
