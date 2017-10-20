set nocompatible               " be iMproved
filetype off                   " required for vundle... we turn it back on later

" turn on persistent undo, and store it in the vim dir
set undofile
set undodir=~/.vim/undodir

" default space and tab handling
set shiftwidth=2
set tabstop=2
set expandtab
set softtabstop=2

" fold by indents unless specified otherwise
set foldmethod=indent
set foldlevel=9999

" use dark themes
set background=dark

" use the mouse in terminal mode
set mouse=a

" backspace over auto-indents, eols, start of lines
set backspace=indent,eol,start

"""" END BILL'S MAGIC CONFIG """"

set number
syntax on

" Highlight the search (*#/)
set hlsearch

" Color scheme
colorscheme alduin

" Change color scheme based on time of day / night (only works after this file
" has been sourced...boo...
let g:alduin_Contract_Vampirism = 1

" Split to the right and below by default
set splitright
set splitbelow

" Ignore case while searching unless caps are present in the search string
set ignorecase
set smartcase


"""" Vundle config """"


" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
" alternatively, pass a path where Vundle should install plugins
"call vundle#begin('~/some/path/here')

" let Vundle manage Vundle, required
Plugin 'VundleVim/Vundle.vim'

" NERDtree
Plugin 'scrooloose/nerdtree'

" Javascript highlighting
Plugin 'pangloss/vim-javascript'

" JSX highlighting 
Plugin 'mxw/vim-jsx'

" Linting
Plugin 'w0rp/ale'

" Fuzzy find files
" NOTE: ripgrep is needed for this: https://github.com/BurntSushi/ripgrep
Plugin 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plugin 'junegunn/fzf.vim'

" Git integration
Plugin 'tpope/vim-fugitive'

" Autocomplete
" This...does not work well at all. Boo.
" Plugin 'valloric/youcompleteme'

" Status bar
Plugin 'bling/vim-airline'

" Git status bar
Plugin 'airblade/vim-gitgutter'


" All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin indent on    " required
" To ignore plugin indent changes, instead use:
"filetype plugin on
"
" Brief help
" :PluginList       - lists configured plugins
" :PluginInstall    - installs plugins; append `!` to update or just :PluginUpdate
" :PluginSearch foo - searches for foo; append `!` to refresh local cache
" :PluginClean      - confirms removal of unused plugins; append `!` to auto-approve removal
"
" see :h vundle for more details or wiki for FAQ
" Put your non-Plugin stuff after this line


"""" Plugin settings """"

" Open NERDTree when vim is started with no options
" Commented out because opening vim with no options gave me an error and I
" don't know why
" autocmd StdinReadPre * let s:std_in=1
" autocmd VimEnter * if argc() == 0 && !exists(“s:std_in”) | NERDTree | endif

" Toggle NERDTree with \\
nnoremap <Leader>\ :NERDTreeToggle<Enter>

" Close a tab if NERDTree is the only window left
" Commented out because closing NERDTree was giving me an error and I don't
" know why
" autocmd bufenter * if (winnr(“$”) == 1 && exists(“b:NERDTreeType”) && b:NERDTreeType == “primary”) | q | endif

" Make NERDTree prettier
let NERDTreeMinimalUI = 1
let NERDTreeDirArrows = 1

" Show hidden files in NERDTree by default
let NERDTreeShowHidden = 1

" ALE (linter) fix problems on save
let g:ale_fixers = {
\   'javascript': ['eslint'],
\}

let g:ale_fix_on_save = 1

"""" Remap keys """"

" Swap pains without control+<direction>
nnoremap <C-J> <C-W>j
nnoremap <C-K> <C-W>k
nnoremap <C-L> <C-W>l
nnoremap <C-H> <C-W>h

" ;l = escape because it's fast and doesn't interfere with ending lines in ;
inoremap ;l <Esc>
vnoremap ;l <Esc>

" Go to linting errors
nnoremap ]e :ALENextWrap<Return>
nnoremap [e :ALEPreviousWrap<Return>

" Copy to clipboard
vnoremap <Leader>y "+y

"""" Define new commands """"

" :Find
" --column: Show column number
" --line-number: Show line number
" --no-heading: Do not show file headings in results
" --fixed-strings: Search term as a literal string
" --ignore-case: Case insensitive search
" --no-ignore: Do not respect .gitignore, etc...
" --hidden: Search hidden files and folders
" --follow: Follow symlinks
" --glob: Additional conditions for search (in this case ignore everything in the .git/ folder)
" --color: Search color options
command! -bang -nargs=* Find call fzf#vim#grep('rg --column --line-number --no-heading --fixed-strings --ignore-case --no-ignore --hidden --follow --glob "!.git/*" --glob "!.node_modules/" --color "always" '.shellescape(<q-args>), 1, <bang>0)

"tab autocompletes
function! InsertTabWrapper()
      let col = col('.') - 1
      if !col || getline('.')[col - 1] !~ '\k'
          return "\<tab>"
      else
          return "\<c-p>"
      endif
endfunction 
inoremap <tab> <c-r>=InsertTabWrapper()<cr>

" \g to fzf (won't work until ripgrep is installed)
command! -bang -nargs=* Rg
  \ call fzf#vim#grep(
  \   'rg --column --line-number --no-heading --color=always '.shellescape(<q-args>), 1,
  \   <bang>0 ? fzf#vim#with_preview('up:60%')
  \           : fzf#vim#with_preview('right:50%:hidden', '?'),
  \   <bang>0)

nnoremap <silent> <leader>g :Rg

