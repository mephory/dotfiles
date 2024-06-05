function SetupHilights()
  vim.api.nvim_set_hl(0, 'MyHilight1', {fg="#000000", ctermfg=16, bg="#ffa724", ctermbg=208})
  vim.api.nvim_set_hl(0, 'MyHilight2', {fg="#000000", ctermfg=16, bg="#aeee00", ctermbg=040})
  vim.api.nvim_set_hl(0, 'MyHilight3', {fg="#000000", ctermfg=16, bg="#8cffba", ctermbg=087})
  vim.api.nvim_set_hl(0, 'MyHilight4', {fg="#000000", ctermfg=16, bg="#b88853", ctermbg=137})
  vim.api.nvim_set_hl(0, 'MyHilight5', {fg="#000000", ctermfg=16, bg="#ff9eb8", ctermbg=211})
  vim.api.nvim_set_hl(0, 'MyHilight6', {fg="#000000", ctermfg=16, bg="#ff2c4b", ctermbg=195})
end

function HilightCWord(n)
  local pattern = vim.fn.expand('<cword>')
  local mid = 86750 + n

  if not pcall(vim.fn.matchdelete, mid) then
    vim.fn.matchadd("MyHilight" .. n, pattern, 1, mid)
  end
end

function HilightPattern(n)
  local mid = 86760 + n

  if not pcall(vim.fn.matchdelete, mid) then
    local pattern = vim.fn.input('Pattern: ')
    vim.fn.matchadd("MyHilight" .. n, pattern, 1, mid)
  end
end

vim.api.nvim_create_autocmd({"OptionSet"}, {
  pattern = {"background"},
  callback = function(ev)
    SetupHilights()
  end
})

SetupHilights()

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
