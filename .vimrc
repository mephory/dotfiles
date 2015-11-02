" TODO
"   maybe   <C-j> move line down
"   maybe   <C-k> move line up
"   maybe   <C-h> move character left
"   maybe   <C-l> move chatarter right
"   <cr> is free!

set nocompatible
filetype off

execute pathogen#infect()

"===============================================================================
" Basic Settings                                                             {{{
"===============================================================================
set encoding=utf-8
set autoindent
set expandtab       " replace tabs with spaces
set tabstop=4       " ... with four spaces
set shiftwidth=4    " ... and do the same for > and <
set history=500     " keep 500 lines of history
set directory=~/.vim/swapfiles
set noesckeys       " disables the delay when hitting escape
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
filetype plugin on

"============================================================================}}}
" Look                                                                       {{{
"===============================================================================
syntax on           " enable syntax highlighting
set number          " show line numbers
set nohlsearch      " don't highlight search matches
set cursorline      " highlight the current line
set t_Co=256
set list listchars=tab:»·,trail:·

set background=dark
colorscheme solarized
set laststatus=2    " always show status bar
set guifont=Inconsolata\ 13

"============================================================================}}}
" Autocmds                                                                   {{{
"===============================================================================
" Filetype Indent 
augroup filetypes
    autocmd!
    autocmd FileType php set ai sw=2 sts=2 et
    autocmd FileType ruby set ai sw=2 sts=2 et
    autocmd FileType lisp set ai sw=2 sts=2 et
    autocmd FileType xml setlocal equalprg=xmllint\ --format\ --recover\ -\ 2>/dev/null    " = for xml
    autocmd BufEnter *.hy set filetype=lisp
    autocmd BufRead COMMIT_EDITMSG setlocal spell!    " enable spell checking for commit msgs
augroup END


augroup templates
    autocmd!
    autocmd BufNewFile *.* silent! execute '0r ~/.vim/templates/'.expand("<afile>:e").'.template' | normal Gddgg
augroup END

"============================================================================}}}
" Plugin-specific Configuration                                              {{{
"===============================================================================
let Tlist_Use_Right_Window = 1          " open tlist on the right
let Tlist_GainFocus_On_ToggleOpen = 1   " focus tlist on first open
let Tlist_Close_On_Select = 1           " exit vim when only the tlist window is opened
let Tlist_Exit_OnlyWindow = 1           " exit vim when only the tlist window is opened

let g:ctrlp_working_path_mode = 2

let g:table_mode_corner_corner='+'
let g:table_mode_header_fillchar='='
let g:table_mode_toggle_map = "q"

let g:airline_powerline_fonts                                 = 0
" don't count trailing whitespace since it lags in huge files
let g:airline#extensions#whitespace#enabled                   = 0
let g:airline_theme                                           = 'custom'
" Just show the filename (no path) in the tab
let g:airline#extensions#tabline#fnamemod                     = ':t'
let g:airline_left_sep                                        = ''
let g:airline_right_sep                                       = ''

let g:user_emmet_mode = 'i'


"============================================================================}}}
" Key Configuration                                                          {{{
"===============================================================================
" Tab and Split Management                                                   {{{
"-------------------------------------------------------------------------------
let mapleader='t'
map <leader>n :tabnew<cr>
map <leader>c :tabclose<cr>
map <leader>t :tabnext<cr>
map <leader>T :tabprevious<cr>
map <leader>p :CtrlPBuffer<cr>
map <leader>m :tabmove 
map <leader>f :tabfirst<cr>
map <leader>l :tablast<cr>
map <leader>o :tabonly<cr>
map <leader>e :tabedit <c-r>=expand("%:p:h")<cr><cr>
map <leader>q :CtrlPTag<cr>
map <C-W>u :call MergeTabs()<cr>
map <C-W><C-U> :call MergeTabs()<cr>
map <BS> <C-W>h
map <C-h> <C-W>h
map <C-j> <C-W>j
map <C-k> <C-W>k
map <C-l> <C-W>l
map <F1> :ls<cr>:b 

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

"----------------------------------------------------------------------------}}}
" Movement                                                                   {{{
"-------------------------------------------------------------------------------
map <leader>, <C-^>

" Open ctags definitions in a new tab
map <C-\> :tab split<CR>:exec("tag ".expand("<cword>"))<CR>

" Use Tab and S-Tab as ; and ,
nnoremap <Tab> ;
nnoremap <S-Tab> ,

" Move up in down in visual mode even with capital J and K
vnoremap K k
vnoremap J j

" zT is like zt but with some space at the top
map zT zt10<C-y>
map zB zb10<C-e>

"----------------------------------------------------------------------------}}}
" File and Project Management                                                {{{
"-------------------------------------------------------------------------------
nnoremap ''w :CtrlP /var/www/<cr>
nnoremap ''c :CtrlP /home/mephory/code/crescent/<cr>
nnoremap ''n :CtrlP /home/mephory/code/nexus/<cr>
nnoremap ''a :CtrlP /home/mephory/code/alexandria/<cr>
nnoremap ''v :e `=resolve(expand("~/.vimrc"))`<cr>
nnoremap ''' :cd %:p:h<cr>

"----------------------------------------------------------------------------}}}
" Format                                                                     {{{
"-------------------------------------------------------------------------------
" Tabularize
map <leader>a= :Tabularize /=<CR>
map <leader>a; :Tabularize /;<CR>
map <leader>a: :Tabularize /:\zs<CR>
map <leader>a, :Tabularize /,<CR>
map <leader>a\| :Tabularize /\|<CR>

" Headlines
nnoremap <leader>h yypVr-k
nnoremap <leader>H yypVr=k

" Retain selection after block indent
vnoremap < <gv
vnoremap > >gv

" Toggle hex view
nnoremap <leader>x :call ToggleHex()<cr>

" make K the opposite of J
nnoremap K kJ

" Start Table Mode
nnoremap <leader>tm :TableModeToggle<cr>

" Sort visual selection
vnoremap <leader>so :sort<cr>

" Comment
noremap <leader>c :TComment<cr>

" Split a line in two
map S i<cr><esc>

" Easier emmet key
imap <C-l> <C-y>,

"----------------------------------------------------------------------------}}}
" Git                                                                        {{{
"-------------------------------------------------------------------------------
" Blame in visual mode (taken from https://github.com/r00k/dotfiles/blob/master/vimrc)
vmap <leader>gb :<C-U>!git blame <C-R>=expand("%:p") <CR> \| sed -n <C-R>=line("'<") <CR>,<C-R>=line("'>") <CR>p <CR>
nmap <leader>gb :!git checkout ''<left>

nnoremap <leader>gs :!git st<cr>
nnoremap <leader>gc :!git commit -a -m ''<left>
nnoremap <leader>gC :!git commit<cr>
nnoremap <leader>gl :!git log<cr>
nnoremap <leader>gd :!git diff<cr>
nnoremap <leader>gD :!git diff 
nnoremap <leader>ga :!git add %<cr>
nnoremap <leader>gp :!git push<cr>
nnoremap <leader>gB :!git branch -a<cr>

"----------------------------------------------------------------------------}}}
" Other                                                                      {{{
"-------------------------------------------------------------------------------
" Insert ƒ when pressing C-f in insert mode
inoremap <c-F> <c-V>u0192

" Open the file openend by gf in a new tab
nnoremap gf <C-w>gF

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
command! W :w !sudo tee %

" Toggle taglist with <leader>t
nnoremap <leader>t :TlistToggle<cr>

" Paste to http://mephory.com/paste
vnoremap <leader>p :call PostSnippet()<cr>
vnoremap <leader>P :call PostSnippet('public')<cr>

" View diff of buffer against original file
nnoremap <leader>d :w !diff -u % -<cr>

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

" use . in visual mode to repeat command on every visually selected line
vnoremap . :norm.<cr>

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
nmap <leader>. :source ~/.vimrc<cr>

vmap <expr>  ++  VMATH_YankAndAnalyse()
nmap         ++  vip++

nmap - :tabnew<cr>:e.<cr>

vmap <cr>j :<C-u>call MoveToEndOfParagraphSameCol()<cr>
vmap <cr>k :<C-u>call MoveToBeginningOfParagraphSameCol()<cr>
nmap <cr>j v<cr>jv
nmap <cr>k v<cr>kv

"----------------------------------------------------------------------------}}}
" Filter through external programs                                           {{{
"-------------------------------------------------------------------------------
let mapleader=' '
nnoremap Q !!$SHELL<cr>
vnoremap Q :!$SHELL<cr>
nnoremap <leader>s !!$SHELL<cr>
vnoremap <leader>s :!$SHELL<cr>
nnoremap <leader>p !!python2 -<cr>
vnoremap <leader>p :!python2 -<cr>
nnoremap <leader>r !!ruby -<cr>
vnoremap <leader>r :!ruby -<cr>

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
    exe "silent !firefox \"".a:url."\""
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
    let b:destination = a:0 ? a:1 : 'mephory.com:/var/www/mephory.com/www/upload/%:t'

    augroup autoscp
        autocmd!
        autocmd BufWritePost <buffer> exe "silent !scp '%' '" . b:destination .  "' 1>/dev/null 2>/dev/null &" | redraw!
    augroup END
endfunction

"============================================================================}}}
