vim.o.encoding = 'utf-8'
vim.o.autoindent = true
vim.o.expandtab = true
vim.o.tabstop = 2
vim.o.shiftwidth = 2
vim.o.history = 500
vim.o.directory = vim.env.HOME .. '/.vim/swapfiles'
vim.o.incsearch = false
vim.o.inccommand = 'split'
vim.o.foldmethod = 'marker'
vim.o.foldlevel = 99
vim.o.wildignore = '*.hi,*.o*.so,*.swp,*.zip,*.jpg,*.png,*.gif,*.pyc'
vim.o.wildmenu = true
vim.o.splitright = true
vim.o.splitbelow = true
vim.o.shell = '/bin/zsh'
vim.o.ignorecase = true
vim.o.smartcase = true
vim.o.spelllang = 'en,de'
vim.o.shiftround = true
vim.o.backspace = 'indent,eol,start'
vim.o.relativenumber = true
vim.o.number = true
vim.o.hlsearch = false
vim.o.cursorline = false
vim.o.laststatus = 2
vim.opt.path:append('**')
vim.opt.matchpairs:append('<:>')
vim.cmd('filetype indent off')

vim.env.NVIM_TUI_ENABLE_TRUE_COLOR = '1'

if (vim.env.WISP_THEME) then
  vim.cmd('colorscheme ' .. vim.env.WISP_THEME)
else
  vim.cmd('colorscheme gruvbox')
end
