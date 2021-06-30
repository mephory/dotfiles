" TODO
"   maybe   <C-j> move line down
"   maybe   <C-k> move line up
"   maybe   <C-h> move character left
"   maybe   <C-l> move chatarter right
"   maybe   vnoremap qq :norm! qq
"   <cr> is free!

set nocompatible

call plug#begin()
" Plug 'neovim/nvim-lspconfig'
Plug 'dhruvasagar/vim-table-mode'
Plug 'groenewege/vim-less'
Plug 'junegunn/fzf.vim'
Plug 'justinmk/vim-sneak'
Plug 'ludovicchabant/vim-gutentags'
Plug 'mattn/emmet-vim'
Plug 'morhetz/gruvbox'
Plug 'romainl/vim-qf'
Plug 'tommcdo/vim-exchange'
Plug 'tommcdo/vim-lion'
Plug 'tpope/vim-abolish'
Plug 'tpope/vim-bundler'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-eunuch'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-rails'
Plug 'tpope/vim-rbenv'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-unimpaired'
Plug 'vim-pandoc/vim-pandoc'
Plug 'vim-pandoc/vim-pandoc-syntax'
Plug 'w0rp/ale'
Plug 'wellle/targets.vim'
Plug 'xolox/vim-misc'
Plug 'arcticicestudio/nord-vim'
Plug '~/code/vim-mtg'
Plug '~/code/vim-mpv'
Plug 'leafgarland/typescript-vim'
Plug 'peitalin/vim-jsx-typescript'
Plug 'neoclide/coc.nvim', { 'branch': 'release' }
call plug#end()

"===============================================================================
" Basic Settings                                                             {{{
"===============================================================================
set encoding=utf-8
set autoindent
set expandtab       " replace tabs with spaces
set tabstop=2       " ... with four spaces
set shiftwidth=2    " ... and do the same for > and <
set history=500     " keep 500 lines of history
set directory=~/.vim/swapfiles
set noincsearch
set inccommand=split
set foldmethod=marker
set foldlevel=99
set wildignore+=*.hi,*.o*.so,*.swp,*.zip,*.jpg,*.png,*.gif,*.pyc
set wildmenu
set splitright splitbelow
set shell=/bin/zsh
set matchpairs+=<:>
set ignorecase
set smartcase
set spelllang=en,de
set shiftround
set backspace=indent,eol,start
set relativenumber
set path+=**
filetype plugin on
filetype indent off

"============================================================================}}}
" LSP                                                                        {{{
"===============================================================================

" let g:lsp_log_verbose = 1
" let g:lsp_log_file = expand('/tmp/lsp-log.txt')

" lua << EOF
"     local nvim_lsp = require'nvim_lsp'

"     nvim_lsp.solargraph.setup{name = "solargraph", cmd = { "solargraph", "stdio" }, settings = {
"         solargraph = {
"             useBundler = true
"         }
"     }}

"     -- nvim_lsp.omnisharp.setup{}
"     nvim_lsp.tsserver.setup{}
" EOF

inoremap <silent><expr> <c-space> coc#refresh()
nnoremap <silent> K :call CocAction('doHover')<cr>
inoremap <silent> <C-l> <cmd>call CocAction('showSignatureHelp')<cr>
noremap <silent> <C-k> <cmd>call CocAction('showSignatureHelp')<cr>
" noremap <silent> gD <cmd>call CocAction('jumpDefinition')<cr>
nmap <silent> gD <Plug>(coc-definition)

" nnoremap K <cmd>lua vim.lsp.buf.hover()<CR>
" nnoremap gD <cmd>lua vim.lsp.buf.definition()<CR>
" nnoremap <C-k> <cmd>lua vim.lsp.buf.signature_help()<CR>
" inoremap <C-l> <cmd>lua vim.lsp.buf.signature_help()<CR>
" inoremap <C-f> <cmd>lua vim.lsp.buf.signature_help()<CR>

"============================================================================}}}
" Look                                                                       {{{
"===============================================================================
syntax on           " enable syntax highlighting
set number          " show line numbers
set nohlsearch      " don't highlight search matches
set cursorline      " highlight the current line
set t_Co=256
set list listchars=tab:»·,trail:·
set t_ut =

colorscheme gruvbox
execute 'colorscheme ' . system('get-xrdb-env THEME')
set background=dark
" colorscheme nord
" set background=light
" set background=dark

set laststatus=2    " always show status bar
set guifont=Inconsolata\ 13

"============================================================================}}}
" Autocmds                                                                   {{{
"===============================================================================
" Filetype Indent 
augroup filetypes
    autocmd!
    autocmd FileType clojure set ai sw=2 sts=2 et
    autocmd FileType php set ai sw=2 sts=2 et
    autocmd FileType ruby set ai sw=2 sts=2 et
    autocmd FileType go set ai sw=2 sts=2 et
    autocmd FileType lisp set ai sw=2 sts=2 et
    autocmd FileType javascript set ai sw=2 sts=2 et
    autocmd FileType javascriptreact set ai sw=2 sts=2 et
    autocmd FileType typescript set ai sw=2 sts=2 et
    autocmd FileType typescriptreact set ai sw=2 sts=2 et

    autocmd FileType xml setlocal equalprg=xmllint\ --format\ --recover\ -\ 2>/dev/null
    autocmd FileType ruby setlocal equalprg=rubocop\ --fail-level\ F\ --stderr\ --except\ Layout/InitialIndentation\ --except\ Style/FrozenStringLiteralComment\ -A\ -s\ /tmp/rubocop-output\ 2\>\/dev\/null
    autocmd FileType cs set ai sw=4 sts=4 et
    autocmd BufEnter *.hy set filetype=lisp

    autocmd BufRead COMMIT_EDITMSG setlocal spell!    " enable spell checking for commit msgs

    autocmd FileType ruby setlocal omnifunc=v:lua.vim.lsp.omnifunc
    autocmd FileType typescript.tsx setlocal omnifunc=v:lua.vim.lsp.omnifunc
    autocmd FileType cs setlocal omnifunc=v:lua.vim.lsp.omnifunc

    autocmd FileType pandoc nmap <buffer> ,n <Plug>(pandoc-keyboard-next-li)
    autocmd FileType pandoc nmap <buffer> ,p <Plug>(pandoc-keyboard-prev-li)
augroup END


" augroup templates
"     autocmd!
"     autocmd BufNewFile *.* silent! execute '0r ~/.vim/templates/'.expand("<afile>:e").'.template' | normal Gddgg
" augroup END

"============================================================================}}}
" Plugin-specific Configuration                                              {{{
"===============================================================================
let g:netrw_banner = 0
" let g:netrw_liststyle = 3

let g:fzf_buffers_jump = 1

let g:table_mode_corner_corner='+'
let g:table_mode_header_fillchar='='
let g:table_mode_toggle_map = "q"

let g:user_emmet_mode = 'i'

let g:pandoc#formatting#mode = 'hA'
let g:pandoc#hypertext#open_editable_alternates = 1

let g:typescript_indent_disable = 1


"============================================================================}}}
" Key Configuration                                                          {{{
"===============================================================================
let maplocalleader='\'
" Tab and Split Management                                                   {{{
"-------------------------------------------------------------------------------
let mapleader='t'
nmap <leader>n :tabnew<cr>
nmap <leader>c :tabclose<cr>
nmap <leader>t :tabnext<cr>
nmap <leader>T :tabprevious<cr>
nmap <leader>p :Buffers<cr>
nmap <leader>m :tabmove 
nmap <leader>f :tabfirst<cr>
nmap <leader>l :tablast<cr>
nmap <leader>o :tabonly<cr>
nmap <leader>e :tabedit <c-r>=expand("%:p:h")<cr><cr>
nmap <leader>q :Tags<cr>
nmap <C-W>u :call MergeTabs()<cr>
nmap <C-W><C-U> :call MergeTabs()<cr>
nmap <BS> <C-W>h
nmap <C-h> <C-W>h
nmap <C-l> <C-W>l
nmap <F1> :ls<cr>:b 
nmap <C-p> :GFiles<cr>
nmap <space>p :Files<cr>


" unmap the above <leader>-mappings in operator pending mode,
" so that I can still do things like dtn
onoremap tn tn
onoremap tc tc
onoremap tt tt
onoremap tT tT
onoremap tp tp
onoremap tm tm
onoremap tf tf
onoremap tl tl
onoremap to to
onoremap te te
onoremap tq tq

let mapleader=','
nnoremap <leader>sp :sp<cr>
nnoremap <leader>v :vsp<cr>
nnoremap <leader>b :ls<cr>:b 

" View buffer in two columns
nnoremap <leader>u gg:vsp<cr>Ljzt:set scrollbind<cr><C-W>h:set scrollbind<cr>

map <F2> vip!rubocop --fail-level F --stderr --except Layout/InitialIndentation -a -s /tmp/rubocop-output 2>/dev/null<cr>
vmap <F2> !rubocop --fail-level F --stderr --except Layout/InitialIndentation -a -s /tmp/rubocop-output 2>/dev/null<cr>

"----------------------------------------------------------------------------}}}
" Movement                                                                   {{{
"-------------------------------------------------------------------------------
map <leader>, <C-^>

" Open ctags definitions in a new tab
map <C-\> :tab split<CR>:exec("tag ".expand("<cword>"))<CR>

" Use Tab and S-Tab as ; and ,
nnoremap <Tab> ;
nnoremap <S-Tab> ,
map <Tab> <Plug>Sneak_;
map <S-Tab> <Plug>Sneak_,
nnoremap <leader>o <C-i>
nnoremap <leader>j :jumps<cr>
nnoremap <PageUp> <C-o>
nnoremap <PageDown> <C-i>

" Move up in down in visual mode even with capital J and K
vnoremap K k
vnoremap J j

" zT is like zt but with some space at the top
map zT zt10<C-y>
map zB zb10<C-e>

"----------------------------------------------------------------------------}}}
" File and Project Management                                                {{{
"-------------------------------------------------------------------------------
nnoremap ''v :e `=resolve(expand("~/.config/nvim/init.vim"))`<cr>
nnoremap ''x :e `=resolve(expand("~/.xmonad/xmonad.hs"))`<cr>
nnoremap ''z :e `=resolve(expand("~/.zshrc"))`<cr>
nnoremap ''w :e `=resolve(expand("~/.wiki/index.pandoc"))`<cr>
nnoremap ''' :cd %:p:h<cr>
nnoremap \e :e <C-r>=expand('%:h')<cr>/
nnoremap \r :r <C-r>=expand('%:h')<cr>/

"----------------------------------------------------------------------------}}}
" Format                                                                     {{{
"-------------------------------------------------------------------------------
" Tabularize
map <leader>a= :Tabularize /=<CR>
map <leader>a; :Tabularize /;<CR>
map <leader>a: :Tabularize /:\zs<CR>
map <leader>a, :Tabularize /,<CR>
map <leader>a\| :Tabularize /\|<CR>
map <leader>aa :Tabularize /
map <leader>A, :Tabularize /^[^,]*,\zs<CR>
map <leader>A: :Tabularize /^[^:]*:\zs<CR>

" Headlines
nnoremap <leader>h yypv$r-k
nnoremap <leader>H yypv$r=k

" Retain selection after block indent
vnoremap < <gv
vnoremap > >gv

" Toggle hex view
nnoremap <leader>x :call ToggleHex()<cr>

" Start Table Mode
nnoremap <leader>tm :TableModeToggle<cr>

" Sort visual selection
vnoremap <leader>so :sort<cr>

" Comment
nmap <leader>c gcc
vmap <leader>c gc

" Split a line in two
map gs i<cr><esc>

" Easier emmet key
" imap <C-l> <C-y>,

"----------------------------------------------------------------------------}}}
" Git                                                                        {{{
"-------------------------------------------------------------------------------
" Blame in visual mode (taken from https://github.com/r00k/dotfiles/blob/master/vimrc)
vmap <leader>gb :<C-U>!git blame <C-R>=expand("%:p") <CR> \| sed -n <C-R>=line("'<") <CR>,<C-R>=line("'>") <CR>p <CR>
vmap <leader>gh :<C-U>!git log -L <C-R>=line("'<")<cr>,<C-R>=line("'>")<cr>:<C-R>=expand("%:p")<cr><cr>
nmap <leader>gb :!git checkout ''<left>

nnoremap <leader>gs :!git st<cr>
nnoremap <leader>gc :!git commit -a -m ''<left>
nnoremap <leader>gC :!git commit<cr>
nnoremap <leader>gl :!git log<cr>
nnoremap <leader>gd :!git diff %<cr>
nnoremap <leader>gD :!git diff 
nnoremap <leader>ga :!git add %<cr>
nnoremap <leader>gp :!git push<cr>
nnoremap <leader>gB :!git branch -a<cr>

"----------------------------------------------------------------------------}}}
" Other                                                                      {{{
"-------------------------------------------------------------------------------
" Open the file openend by gf in a new tab
" nnoremap gf <C-w>gF

" Y yanks until the end of line
nnoremap Y y$

" Open a quickfix window for the last search
"  ,/ searches only the current file
"  ,? recursively searched the current folder
nnoremap <leader>/ :call GrepCurrentFile()<cr>
nnoremap <leader>? :call GrepRecursive()<cr>
nnoremap <leader>;/ :execute 'vimgrep /'.@/.'/g %'<cr>:copen<cr>
nnoremap <leader>;? :execute 'grep "'.@/.'" ./ -ir'<cr>:copen<cr>

" :W to save as root
command!-nargs=? W :call SaveAsRoot(<args>)

" Toggle taglist with <leader>t
nnoremap <leader>t :TlistToggle<cr>

" Paste to http://mephory.com/paste
vnoremap <leader>p :call PostSnippet()<cr>
vnoremap <leader>P :call PostSnippet('public')<cr>

" View diff of buffer against original file
nnoremap <leader>d :w !git diff --no-index % -<cr>

vnoremap <leader>y :w !xsel -ib<cr><cr>

" Improve command line navigation a bit
cnoremap <C-a> <Home>
cnoremap <C-e> <End>

" Toggle paste
set pastetoggle=<F8>

" Quit with Q *or* q
command! Q q

" Use + and - for incrementing and decrementing a number
nnoremap <C-+> <C-a>
nnoremap <C--> <C-x>

nnoremap <leader>w :set list!<cr>

" one rarely wants visual mode, so bind v to visual block mode instead
nnoremap    v   <C-V>
nnoremap <C-V>     v

vnoremap    v   <C-V>
vnoremap <C-V>     v

" use ; as : to make colon commands easier to type
nnoremap  ;  :
nnoremap q; q:

" use . in visual mode to repeat command on every visually selected line
vnoremap . :norm.<cr>

" add current line to quickfix
nnoremap <leader>q :caddexpr expand("%") . ":" . line(".") . ":" . getline(".")<cr>
autocmd FileType qf nnoremap <buffer> <silent> dd
  \ :call setqflist(filter(getqflist(), {idx -> idx != line('.') - 1}), 'r') <Bar> cc<CR>

" hilight words in different colors
hi def InterestingWord1 guifg=#000000 ctermfg=16 guibg=#ffa724 ctermbg=208
hi def InterestingWord2 guifg=#000000 ctermfg=16 guibg=#aeee00 ctermbg=040
hi def InterestingWord3 guifg=#000000 ctermfg=16 guibg=#8cffba ctermbg=087
hi def InterestingWord4 guifg=#000000 ctermfg=16 guibg=#b88853 ctermbg=137
hi def InterestingWord5 guifg=#000000 ctermfg=16 guibg=#ff9eb8 ctermbg=211
hi def InterestingWord6 guifg=#000000 ctermfg=16 guibg=#ff2c4b ctermbg=195
nnoremap <silent> <leader>0 :call clearmatches()<cr>
nnoremap <silent> <leader>1 :call HiInterestingWord(1)<cr>
nnoremap <silent> <leader>2 :call HiInterestingWord(2)<cr>
nnoremap <silent> <leader>3 :call HiInterestingWord(3)<cr>
nnoremap <silent> <leader>4 :call HiInterestingWord(4)<cr>
nnoremap <silent> <leader>5 :call HiInterestingWord(5)<cr>
nnoremap <silent> <leader>6 :call HiInterestingWord(6)<cr>
nnoremap <silent> <leader>! :call HiPattern(1)<cr>
nnoremap <silent> <leader>@ :call HiPattern(2)<cr>
nnoremap <silent> <leader># :call HiPattern(3)<cr>
nnoremap <silent> <leader>$ :call HiPattern(4)<cr>
nnoremap <silent> <leader>% :call HiPattern(5)<cr>
nnoremap <silent> <leader>^ :call HiPattern(6)<cr>
nnoremap <silent> <leader>& :call HiPattern(7)<cr>
nnoremap <silent> <leader>& :call HiPattern(8)<cr>
nnoremap <silent> <leader>& :call HiPattern(9)<cr>

" reload vimrc
nmap <leader>. :source ~/.config/nvim/init.vim<cr>

vmap <expr>  ++  VMATH_YankAndAnalyse()
nmap         ++  vip++

nmap - :tabnew<cr>:e.<cr>

vmap <space>j :<C-u>call MoveToEndOfParagraphSameCol()<cr>
vmap <space>k :<C-u>call MoveToBeginningOfParagraphSameCol()<cr>
nmap <space>j v<space>jv
nmap <space>k v<space>kv

"----------------------------------------------------------------------------}}}
" Filter through external programs                                           {{{
"-------------------------------------------------------------------------------
let mapleader=' '
nnoremap Q !!$SHELL<cr>
vnoremap Q :!$SHELL<cr>
nnoremap <leader>s !!$SHELL<cr>
vnoremap <leader>s :!$SHELL<cr>
" nnoremap <leader>p !!python2 -<cr>
" vnoremap <leader>p :!python2 -<cr>
nnoremap <leader>q :%!ruby -<cr>
nnoremap <leader>r !!ruby -<cr>
vnoremap <leader>r :!ruby -<cr>
" nnoremap <leader>e :!%<cr>

nmap <leader><space> ml<space>su`l
vmap <leader><space> ml<space>su`l
nmap <leader>v mlvip<space>su`l

" ... execute visual selection, but not line-wise
vmap <leader>c dmlo<esc>p<space>s"lDdd`lh"lp
vmap <leader>e dmlo<esc>p!!ruby -e 'print(eval(STDIN.read))'<cr>"lDdd`lh"lp

nnoremap <leader>R :call PipeToProgram('ruby', 'rb')<cr>
vnoremap <leader>R :<C-u>call VisualPipeToProgram('ruby', 'rb')<cr>
nnoremap <leader>P :call PipeToProgram('python2', 'py')<cr>
vnoremap <leader>P :<C-u>call VisualPipeToProgram('python2', 'py')<cr>
nnoremap <leader>S :call PipeToProgram('/bin/zsh', 'sh')<cr>
vnoremap <leader>S :<C-u>call VisualPipeToProgram('/bin/zsh', 'sh')<cr>
" }}}

"============================================================================}}}
" Custom Functions                                                           {{{
"===============================================================================
function! PipeToProgram(prog, ext)
  " TODO: Don't do anything if the file is empty

  " let scripttmpfile = system('echo $(mktemp vim.XXXXX).' . a:ext)
  let scripttmpfile = "/tmp/vimpipe." . a:ext
  exe ":silent !vim " . scripttmpfile

  if match(readfile(scripttmpfile), "[^\s]") != -1
    exe ".!" . a:prog . " " . scripttmpfile
  endif

  redraw!
endfunction

function! VisualPipeToProgram(prog, ext)
  " TODO: Don't do anything if the file is empty

  " let scripttmpfile = system('echo $(mktemp vim.XXXXX).' . a:ext)
  let scripttmpfile = "/tmp/vimpipe." . a:ext
  exe ":silent !vim " . scripttmpfile

  if match(readfile(scripttmpfile), "[^\s]") != -1
    exe ":'<,'>!" . a:prog . " " . scripttmpfile
  endif

  redraw!
endfunction

function! MoveToEndOfParagraphSameCol()
    let initial_vstart = getpos("'<")
    let initial_vend = getpos("'>")
    normal! }k
    let endline = line('.')

    call setpos("'<", initial_vstart)
    call setpos("'>", [0, endline, initial_vend[2]])
    normal! gv
    " call cursor(l, c)
endfunction

function! MoveToBeginningOfParagraphSameCol()
    let initial_vstart = getpos("'<")
    let initial_vend = getpos("'>")
    normal! {j
    let endline = line('.')

    call setpos("'<", initial_vstart)
    call setpos("'>", [0, endline, initial_vend[2]])
    normal! gv
    " call cursor(l, c)
endfunction

" Merge a tab into a split in the previous window
function! MergeTabs()
    if tabpagenr() == 1
        return
    endif
    let bufferName = bufname("%")
    if tabpagenr("$") == tabpagenr()
        close!
    else
        close!
        tabprev
    endif
    vsplit
    execute "buffer " . bufferName
endfunction

function! UnmergeTabs()
    let bufferName = bufname("%")
    close!
    tabnew!
    execute "buffer " . bufferName
endfunction

" Grep the current file and open quickfix window
function! GrepCurrentFile()
    let grepquery = input("Search for: ")
    execute 'vimgrep /'.grepquery.'/g %'
    copen
endfunction

" Grep the whole directory recursively
function! GrepRecursive()
    let grepquery = input("Search for: ")
    execute 'grep "'.grepquery.'" ./ -ir'
    copen
endfunction

" Post a snippet to http://paste.mephory.com
" Call with parameter to post public snippet
function! PostSnippet(...) range
    let tempFile = tempname()
    call writefile(getline(a:firstline, a:lastline), tempFile)

    if a:0
        let PS_Url = system('snippet -p -s '.&syntax.' -n "excerpt from ' .  expand('%:t') . '" '.tempFile)
    else
        let PS_Url = system('snippet -s '.&syntax.' -n "excerpt from ' .  expand('%:t') . '" '.tempFile)
    end

    echom PS_Url
endfunction

" Toggle Hex Edit Mode
function! ToggleHexEdit()
    if (b:hexedit_mode == 'yes')
        :%!xxd -r
        let b:hexedit_mode = 'no'
    else
        :%!xxd
        let b:hexedit_mode = 'yes'
    endif
endfunction

" helper function to toggle hex mode
function! ToggleHex()
  " hex mode should be considered a read-only operation
  " save values for modified and read-only for restoration later,
  " and clear the read-only flag for now
  let l:modified=&mod
  let l:oldreadonly=&readonly
  let &readonly=0
  let l:oldmodifiable=&modifiable
  let &modifiable=1
  if !exists("b:editHex") || !b:editHex
    " save old options
    let b:oldft=&ft
    let b:oldbin=&bin
    " set new options
    setlocal binary " make sure it overrides any textwidth, etc.
    let &ft="xxd"
    " set status
    let b:editHex=1

    " store cursor position
    let b:storedCursorPos = getpos('.')

    " switch to hex editor
    %!xxd

    " restore cursor position
    call cursor(b:storedCursorPos[1], b:storedCursorPos[2])
  else
    " restore old options
    let &ft=b:oldft
    if !b:oldbin
      setlocal nobinary
    endif
    " set status
    let b:editHex=0

    " store cursor position
    let b:storedCursorPos = getpos('.')

    " return to normal editing
    %!xxd -r

    " restore cursor position
    if exists('b:storedCursorPos') 
        call cursor(b:storedCursorPos[1], b:storedCursorPos[2]) 
    endif

  endif
  " restore values for modified and read only state
  let &mod=l:modified
  let &readonly=l:oldreadonly
  let &modifiable=l:oldmodifiable
endfunction

function! OpenURL(url)
    exe "silent !qutebrowser \"".a:url."\""
    redraw!
endfunction
command! -nargs=1 OpenURL :call OpenURL(<q-args>)

" Highlight Word
"
" This mini-plugin provides a few mappings for highlighting words temporarily.
"
" Sometimes you're looking at a hairy piece of code and would like a certain
" word or two to stand out temporarily.  You can search for it, but that only
" gives you one color of highlighting.  Now you can use <leader>N where N is
" a number from 1-6 to highlight the current word in a specific color.

function! HiInterestingWord(n)
    " TODO: Toggle doesn't really work if, because the pattern being matched
    " isn't checked

    " Save our location.
    normal! mz

    " Yank the current word into the z register.
    normal! "zyiw

    " Calculate an arbitrary match ID.  Hopefully nothing else is using it.
    " let mid = 86750 + a:n
    let mid = 86750 + a:n

    " Clear existing matches, but don't worry if they don't exist.
    " silent! call matchdelete(mid)
    silent! let has_previous_match = matchdelete(mid)

    " Construct a literal pattern that has to match at boundaries.
    let pat = '\V\<' . escape(@z, '\') . '\>'

    if has_previous_match == -1
        " Actually match the words.
        call matchadd("InterestingWord" . a:n, pat, 1, mid)
    endif

    " Move back to our original location.
    normal! `z
endfunction

function! HiPattern(n)
  let mid = 86760 + a:n

  silent! let has_previous_match = matchdelete(mid)

  if has_previous_match == -1
    let re = input("Pattern: ")
    call matchadd("InterestingWord" . a:n, re, 1, mid)
  end
endfunction

function! AutoScp(...)
    let b:destination = a:0 ? a:1 : 'mephory.com:shared/public/%:t'

    augroup autoscp
        autocmd!
        autocmd BufWritePost <buffer> exe "silent !scp '%' '" . b:destination .  "' 1>/dev/null 2>/dev/null &" | redraw!
    augroup END
endfunction

function! PandocMode()
    let b:pandoc_command = a:0 ? a:1 : 'pandoc % --self-contained -c ~/.config/pandoc.css --quiet -o /tmp/pandoc-mode/%:t.html'
    let b:refresh_command = 'xdotool search --name qutebrowser windowactivate --sync key r windowactivate $(xdotool getactivewindow)'

    silent! exe '!mkdir -p /tmp/pandoc-mode'
    silent! exe '!touch /tmp/pandoc-mode/%:t.html'
    silent! exe '!qutebrowser /tmp/pandoc-mode/%:t.html'
    redraw!

    augroup pandocmode
        autocmd!
        autocmd BufWritePost <buffer> exe "silent !(" . b:pandoc_command . ' && ' . b:refresh_command . ') &'
    augroup END
endfunction

function! NoPandocMode()
  augroup pandocmode
    autocmd!
  augroup END
endfunction

function! AutoRefresh()
  let b:command = 'xdotool search --name qutebrowser key r'
  augroup autorefresh
    autocmd!
    autocmd BufWritePost <buffer> exe "silent !" . b:command . " &"
  augroup END
endfunction

function! NoAutoRefresh()
  augroup autorefresh
    autocmd!
  augroup END
endfunction

function! SaveAsRoot() abort
  " TODO: Wenn args, dann dahin speichern statt nach %

  :silent! w !env SUDO_EDITOR=tee sudo -e % >/dev/null
  let &modified = v:shell_error
  :e %
endfunction

onoremap <silent>ai :<C-U>cal <SID>IndTxtObj(0)<CR>
onoremap <silent>ii :<C-U>cal <SID>IndTxtObj(1)<CR>
vnoremap <silent>ai :<C-U>cal <SID>IndTxtObj(0)<CR><Esc>gv
vnoremap <silent>ii :<C-U>cal <SID>IndTxtObj(1)<CR><Esc>gv

function! s:IndTxtObj(inner)
  let curline = line(".")
  let lastline = line("$")
  let i = indent(line(".")) - &shiftwidth * (v:count1 - 1)
  let i = i < 0 ? 0 : i
  if getline(".") !~ "^\\s*$"
    let p = line(".") - 1
    let nextblank = getline(p) =~ "^\\s*$"
    while p > 0 && ((i == 0 && !nextblank) || (i > 0 && ((indent(p) >= i && !(nextblank && a:inner)) || (nextblank && !a:inner))))
      -
      let p = line(".") - 1
      let nextblank = getline(p) =~ "^\\s*$"
    endwhile
    normal! 0V
    call cursor(curline, 0)
    let p = line(".") + 1
    let nextblank = getline(p) =~ "^\\s*$"
    while p <= lastline && ((i == 0 && !nextblank) || (i > 0 && ((indent(p) >= i && !(nextblank && a:inner)) || (nextblank && !a:inner))))
      +
      let p = line(".") + 1
      let nextblank = getline(p) =~ "^\\s*$"
    endwhile
    normal! $
  endif
endfunction

function! MtgTest()
  vertical new
  setlocal buftype=nofile bufhidden=wipe nobuflisted noswapfile nowrap
  call setline(1, 'Test')
  setlocal nomodifiable
  nnoremap <nowait> <buffer> q :q<cr>
  1
endfunction

function! s:testf(...) abort
  if !a:0
    let &operatorfunc = matchstr(expand('<sfile>'), '[^. ]*$')
    return 'g@'
  else
    if a:1 == 'line'
      let [l1, l2] = [line("'["), line("']")]
      let result = system("ruby -e 'print(eval(STDIN.read))'", join(getline(l1, l2), "\n"))

      call setline(l2, result)
      if (l1 != l2)
        :exe (l1 . ',' . (l2-1) . 'delete')
      endif 
    else
      let [b1, l1, c1, o1] = getpos("'[")
      let [b2, l2, c2, o2] = getpos("']")
      let l = getline(l1)
      let result = system("ruby -e 'print(eval(STDIN.read))'", l[c1-1:c2])

      if c1 == 1
        let r = result . l[c2:]
      else
        let r = l[:c1-2] . result . l[c2:]
      end

      call setline(l1, r)
    end
  endif
endfunction

nmap <expr> <space>r <SID>testf()
nmap <expr> <space>rr <SID>testf() . '_'
xmap <expr> <space>r <SID>testf()


"============================================================================}}}
