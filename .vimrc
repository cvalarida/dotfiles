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

" use the mouse in terminal mode
set mouse=a

" backspace over auto-indents, eols, start of lines
set backspace=indent,eol,start

" turn off swap files
set nobackup
set nowritebackup
set noswapfile

set number relativenumber

" Use relative numbers for the active window, absolute for inactive
augroup numbertoggle
  autocmd!
  autocmd BufEnter,FocusGained,InsertLeave * set relativenumber
  autocmd BufLeave,FocusLost,InsertEnter   * set norelativenumber
augroup END

" Highlight the search (*#/)
set hlsearch

" Highlight /pattern while typing
set incsearch

" Split to the right and below by default
set splitright
set splitbelow

" Ignore case while searching unless caps are present in the search string
set ignorecase
set smartcase


"""" Plug config """"

" Add fzf to the runtime path
set rtp+=~/.fzf

call plug#begin('~/.vim/plugged')

" NERDtree
Plug 'scrooloose/nerdtree'

" Javascript highlighting
Plug 'pangloss/vim-javascript'

" JSX highlighting 
Plug 'mxw/vim-jsx'

" Linting
Plug 'w0rp/ale'

" Fuzzy find files
" NOTE: ripgrep is needed for this: https://github.com/BurntSushi/ripgrep
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'

" Git integration
Plug 'tpope/vim-fugitive'

" Status bar
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

" Git status bar
Plug 'airblade/vim-gitgutter'

" Golang stuff
Plug 'fatih/vim-go'

" Wrap stuff more easily
Plug 'tpope/vim-surround'

" Auto-close things like ),},], etc.
Plug 'raimondi/delimitmate'

" Vim wiki
Plug 'vimwiki/vimwiki'

" Color scheme
Plug 'morhetz/gruvbox'

" Window swap
Plug 'https://github.com/wesQ3/vim-windowswap'

" Language server
Plug 'autozimu/LanguageClient-neovim', {
      \ 'branch': 'next',
      \ 'do': 'bash install.sh',
      \ }

" Autocomplete
if has('nvim')
  Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
else
  Plug 'Shougo/deoplete.nvim'
  Plug 'roxma/nvim-yarp'
  Plug 'roxma/vim-hug-neovim-rpc'
endif
let g:deoplete#enable_at_startup = 1


" All of your Plugins must be added before the following line
call plug#end()            " required

" Now that we have jsx highlighting, turn syntax on
syntax on

" use dark themes
set background=dark

colorscheme gruvbox

"""" Plugin settings """"

" Open NERDTree when vim is started with no options
" Commented out because opening vim with no options gave me an error and I
" don't know why
" autocmd StdinReadPre * let s:std_in=1
" autocmd VimEnter * if argc() == 0 && !exists(“s:std_in”) | NERDTree | endif

" Toggle NERDTree with \\
nnoremap <Leader>\ :NERDTreeToggle<Enter>

" Reveal current file in NERDTree
nnoremap <Leader>t :NERDTreeFind %<Enter>

" Close a tab if NERDTree is the only window left
" Commented out because closing NERDTree was giving me an error and I don't
" know why
" autocmd bufenter * if (winnr(“$”) == 1 && exists(“b:NERDTreeType”) && b:NERDTreeType == “primary”) | q | endif

" Make NERDTree prettier
let NERDTreeMinimalUI = 1
let NERDTreeDirArrows = 1

" Show hidden files in NERDTree by default
let NERDTreeShowHidden = 1

" Enable airline tabline
let g:airline#extensions#tabline#enabled = 1

" ALE (linter) fix problems on save
" let g:ale_fixers = {
" \   'javascript': ['eslint'],
" \}

let g:ale_fix_on_save = 1
" Without the following line, ale was swallowing my cursor mysteriously
" I don't know _why_ this works or if it'll mess anything else up, so YMMV
" let g:ale_echo_cursor = 0

" When we enter {} (and friends), then hit enter, it expands them like I want
" it to
let g:delimitMate_expand_cr = 1
let g:delimitMate_expand_space = 1

" Airline theme
let g:airline_theme='badwolf'

" Vim wiki and windowswap share <leader>ww so let's remap windowswap
let g:windowswap_map_keys=0
nnoremap <silent> <leader>sw :call WindowSwap#EasyWindowSwap()<Return>

" Language server settings
" The javascript language server will need to be installed first:
" yarn global add javascript-typescript-langserver
" https://fortes.com/2017/language-server-neovim/
" let g:LanguageClient_serverCommands = {
"     \ 'javascript': ['javascript-typescript-stdio'],
"     \ 'javascript.jsx': ['tcp://127.0.0.1:2089'],
"     \ }

" Minimal LanguageServer Protocol configuration for JavaScript
let g:LanguageClient_autoStart = 0
let g:LanguageClient_serverCommands = {}
if executable('javascript-typescript-stdio')
  " let g:LanguageClient_serverCommands.javascript = ['javascript-typescript-stdio']
  " Hopefully this jsx works...
  let g:LanguageClient_serverCommands = {
        \ 'javascript': ['javascript-typescript-stdio'],
        \ 'javascript.jsx': ['javascript-typescript-stdio'],
        \ }
    let g:LanguageClient_rootMarkers = { 'javascript': ['project.json'] }
  " Use LanguageServer for omnifunc completion
  autocmd FileType javascript setlocal omnifunc=LanguageClient#complete
else
  echo "javascript-typescript-stdio not installed!\n"
  :cq
endif

"""" Remap keys """"

" Swap splits with control+<direction>
nnoremap <C-J> <C-W>j
nnoremap <C-K> <C-W>k
nnoremap <C-L> <C-W>l
nnoremap <C-H> <C-W>h

" Resize splits (not sure if I'm too fond of these yet)
nnoremap <leader>- <C-W>-
nnoremap <leader>+ <C-W>+
nnoremap <leader>< <C-W><
nnoremap <leader>> <C-W>>

" ;l = escape because it's fast and doesn't interfere with ending lines in ;
inoremap ;l <Esc>
vnoremap ;l <Esc>

" Go to linting errors
nnoremap ]e :ALENextWrap<Return>
nnoremap [e :ALEPreviousWrap<Return>

" Unfortunately, with g:ale_echo_cursor = 0, linting error text isn't displayed,
" so the following command can be used to open up a new split with the details.
" Don't forget `q` closes (no `:` necessary)
nnoremap <leader>d :ALEDetail<Return>

" Copy to clipboard
vnoremap <Leader>y "+y

" Paste with context -- alternative to https://github.com/sickill/vim-pasta
nnoremap <leader>p p`[v`]=
nnoremap <leader>P P`[v`]=

" Format whole json file with jq
" TODO: Figure out how to only apply this to the line the cursor is on
nnoremap <leader>j :%!jq '.'<Return>

" Open path in vertical split
nnoremap <C-W><C-F> <C-W>vgf

" Copy whole file to clipboard
nnoremap <leader>a gg"+yG''

" Delete previous word
" TODO: Figure out why this doesn't work
" http://vim.wikia.com/wiki/Map_Ctrl-Backspace_to_delete_previous_word
inoremap <C-BS> <C-w>

" Open previously-viewed buffer
" This is sometimes more useful than <c-o> and <c-i> to navigate the jumplist
nnoremap <silent> <M-Left> :b #<Return>

" LanguageServer commands
nnoremap <silent> K :call LanguageClient#textDocument_hover()<Return>
nnoremap <silent> <leader>gd :call LanguageClient#textDocument_definition()<Return>
nnoremap <silent> <F2> :call LanguageClient#textDocument_rename()<Return>
nnoremap <silent> <leader>r :call LanguageClient#textDocument_references()<Return>

" Get outa terminal!
tnoremap <C-h> <C-\><C-n><C-w>h
tnoremap <C-j> <C-\><C-n><C-w>j
tnoremap <C-k> <C-\><C-n><C-w>k
tnoremap <C-l> <C-\><C-n><C-w>l

"""" Define new commands """"

"tab autocompletes
function! InsertTabWrapper()
  let col = col('.') - 1
  if !col || getline('.')[col - 1] !~ '\k'
    return "\<tab>"
  else
    " We're trying out Deoplete with LanguageClient for autocompleting
    if exists('l:omnifunc')
      return "\<c-x>\<c-o>"
    else
      return "\<c-p>"
    endif
  endif
endfunction 
inoremap <tab> <c-r>=InsertTabWrapper()<cr>

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
command! -bang -nargs=* Find call fzf#vim#grep('rg --column --line-number --no-heading --fixed-strings --ignore-case --no-ignore --hidden --follow --glob "!.git/*" --glob "!node_modules/*" --glob "!.babelcache/*" --color "always" '.shellescape(<q-args>), 1, <bang>0)

" \f to fzf
command! -bang -nargs=* Rg
  \ call fzf#vim#grep(
  \   'rg --column --line-number --no-heading --ignore-case --no-ignore --hidden --follow --glob "!.git/*" --glob "!node_modules/*" --glob "!.babelcache/*" --color "always" '.shellescape(<q-args>), 1,
  \   <bang>0 ? fzf#vim#with_preview('up:60%')
  \           : fzf#vim#with_preview('right:50%:hidden', '?'),
  \   <bang>0)

" Find in files
nnoremap <silent> <leader>f :Rg<cr>

" Search for file name
nnoremap <silent> <leader>g :Files<cr>

" Open interactive buffer list
nnoremap <silent> <leader>b :Buffers<cr>

" \zj \zk to jump to next and previous closed folds
nnoremap <silent> <leader>zj :call NextClosedFold('j')<cr>
nnoremap <silent> <leader>zk :call NextClosedFold('k')<cr>
function! NextClosedFold(dir)
    let cmd = 'norm!z' . a:dir
    let view = winsaveview()
    let [l0, l, open] = [0, view.lnum, 1]
    while l != l0 && open
        exe cmd
        let [l0, l] = [l, line('.')]
        let open = foldclosed(l) < 0
    endwhile
    if open
        call winrestview(view)
    endif
endfunction


" camelCase <-> snake_case converter
function! ConvertVariableCase()
  " Find the word under the cursor
  let l:word = expand("<cword>")

  " Change the case
  if l:word =~ '_'
    " The word is snake_case
    let l:sub = substitute(l:word, '_\([a-z]\)', '\=toupper(submatch(1))', 'g')
  else
    " The word is camelCase
    let l:sub = substitute(l:word, '\C\([A-Z]\{1\}\)',
          \ {m -> '_' . tolower(m[1])}, 'g')
  endif

  " Substitute the current word for the new one
  "  (without mangling the registers)
  let l:saved_clipboard = &clipboard
  silent set clipboard "
  let l:saved_reg = getreg('"')
  let l:saved_reg_type = getregtype('"')
  call setreg('"', l:sub)
  " And finally, replace!
  normal! viwp
  " Clean up the registers
  call setreg('"', l:saved_reg, l:saved_reg_type)
  let &clipboard = l:saved_clipboard
endfunction

nnoremap <silent> <leader>c :call ConvertVariableCase()<Return>


" Copy file path to clipboard
function! CopyPathToClipboard()
  let @+ = expand("%")
  echo "Copied path to clipboard:" @+
endfunction

nnoremap <silent> <leader><C-G> :call CopyPathToClipboard()<Return>


" Go directly to certain tabs
for tabNum in [1, 2, 3, 4, 5, 6, 7, 8]
  " Couldn't get <C-1> etc. to work; using <leader> instead
  execute "nnoremap <leader>" . tabNum . " " . tabNum . "gt"
endfor
" Go to last tab
nnoremap <silent> <leader>9 :tablast<Return>


" This is a work in progress! It currently doesn't work even a little bit.
" Search in files that have changed on this branch
" Depends on the alias `git files`
function! BranchGrep(...)
  " Check to see if we have an argument we're searching for
  if a:000 == []
    echo "Please specify a search parameter"
    return
  endif
  " Check to see if we have any files that are different in this branch
  " TODO: There's got to be a better way to get the output of a command than this...
  redir => raw_output
  execute '!git files'
  redir END

  " Find the file names
  " Start by making the string a list so we can remove some lines
  let list_output = split(raw_output, "/\n")
  echo 'List output:'
  echo list_output
  " Abort if we don't have any files that have changed
  " Append our search params
  " Call :grep with the search arguments, searching in the changed files
  " call(grep, search_params)
endfunction

command! Bgrep call BranchGrep(<args>)
