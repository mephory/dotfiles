vim.highlight.create('MyHighlight1', {guifg="#000000", ctermfg=16, guibg="#ffa724", ctermbg=208}, false)
vim.highlight.create('MyHighlight2', {guifg="#000000", ctermfg=16, guibg="#aeee00", ctermbg=040}, false)
vim.highlight.create('MyHighlight3', {guifg="#000000", ctermfg=16, guibg="#8cffba", ctermbg=087}, false)
vim.highlight.create('MyHighlight4', {guifg="#000000", ctermfg=16, guibg="#b88853", ctermbg=137}, false)
vim.highlight.create('MyHighlight5', {guifg="#000000", ctermfg=16, guibg="#ff9eb8", ctermbg=211}, false)
vim.highlight.create('MyHighlight6', {guifg="#000000", ctermfg=16, guibg="#ff2c4b", ctermbg=195}, false)

function HilightCWord(n)
  local pattern = vim.fn.expand('<cword>')
  local mid = 86750 + n

  if not pcall(vim.fn.matchdelete, mid) then
    vim.fn.matchadd("MyHighlight" .. n, pattern, 1, mid)
  end
end

function HilightPattern(n)
  local mid = 86760 + n

  if not pcall(vim.fn.matchdelete, mid) then
    local pattern = vim.fn.input('Pattern: ')
    vim.fn.matchadd("MyHighlight" .. n, pattern, 1, mid)
  end
end

vim.keymap.set('n', ',1', function() HilightCWord(1) end)
vim.keymap.set('n', ',2', function() HilightCWord(2) end)
vim.keymap.set('n', ',3', function() HilightCWord(3) end)
vim.keymap.set('n', ',4', function() HilightCWord(4) end)
vim.keymap.set('n', ',5', function() HilightCWord(5) end)
vim.keymap.set('n', ',6', function() HilightCWord(6) end)
vim.keymap.set('n', ',!', function() HilightPattern(1) end)
vim.keymap.set('n', ',@', function() HilightPattern(2) end)
vim.keymap.set('n', ',#', function() HilightPattern(3) end)
vim.keymap.set('n', ',$', function() HilightPattern(4) end)
vim.keymap.set('n', ',%', function() HilightPattern(5) end)
vim.keymap.set('n', ',^', function() HilightPattern(6) end)
