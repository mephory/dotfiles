function! LocalizeResource()
  let filename = system("notes-helper localize_resource '" . expand("<cfile>") . "' '" . expand("%:p") . "'")
  call setline(".", substitute(getline("."), expand("<cfile>"), filename, ""))
endfunction

function! MoveResource()
  let new_name = input("New Resource Name: ")
  let filename = system("notes-helper move_resource '" . expand("<cfile>") . "' '" . expand("%:p") . "' '" . new_name . "'")
  call setline(".", substitute(getline("."), expand("<cfile>"), filename, ""))
endfunction

nmap <space>i a[](<c-r>+)<esc>F]
nmap <space>l :call LocalizeResource()<cr>
nmap <space>n :Telescope find_files cwd=~/notes<cr>
nmap <space>N :Telescope live_grep cwd=~/notes<cr>
nmap <space>o :call PandocModePdf()<cr>
nmap <space>O :call NoPandocMode()<cr>
nmap <space>m :call MoveResource()<cr>
nmap <space>f :e %:h/<cfile><cr>
nmap <space>e :e ~/notes/<c-d>
imap <F5> <c-r>=strftime("%Y-%m-%d %H:%M:%S")<cr>
imap <F6> <c-r>=strftime("%Y-%m-%d")<cr>
imap <F7> <c-r>=strftime("%H:%M:%S")<cr>
nmap <space>x :silent exe "!xdg-open " . shellescape(expand('%:h') . '/' . expand('<cfile>')) . " &"<cr>
