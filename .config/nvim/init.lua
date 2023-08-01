vim.g.mapleader = ' '

-- Install lazy
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable", -- latest stable release
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)

require('lazy').setup('plugins')

-- [[ Setting options ]]
-- See `:help vim.o`
vim.o.expandtab = true
vim.o.tabstop = 2
vim.o.shiftwidth = 2

-- Set highlight on search
vim.o.hlsearch = false

-- Make line numbers default
vim.wo.number = true

-- Enable mouse mode
vim.o.mouse = ''

-- Enable break indent
vim.o.breakindent = true
vim.o.autoindent = true

-- Save undo history
vim.o.undofile = true

-- Case insensitive searching UNLESS /C or capital in search
vim.o.ignorecase = true
vim.o.smartcase = true

-- Decrease update time
vim.o.updatetime = 250
vim.wo.signcolumn = 'yes'

vim.o.showbreak = '«'

-- Set colorscheme
vim.o.termguicolors = true
vim.cmd [[colorscheme catppuccin-mocha]]

-- Set completeopt to have a better completion experience
vim.o.completeopt = 'menuone,noselect'

-- Keymaps for better default experience
-- See `:help vim.keymap.set()`
vim.keymap.set({ 'n', 'v' }, '<Space>', '<Nop>', { silent = true })

-- Remap for dealing with word wrap
vim.keymap.set('n', 'k', "v:count == 0 ? 'gk' : 'k'", { expr = true, silent = true })
vim.keymap.set('n', 'j', "v:count == 0 ? 'gj' : 'j'", { expr = true, silent = true })

-- [[ Highlight on yank ]]
-- See `:help vim.highlight.on_yank()`
local highlight_group = vim.api.nvim_create_augroup('YankHighlight', { clear = true })
vim.api.nvim_create_autocmd('TextYankPost', {
  callback = function()
    vim.highlight.on_yank()
  end,
  group = highlight_group,
  pattern = '*',
})

-- Set lualine as statusline
-- See `:help lualine.txt`
require('lualine').setup {
  options = {
    icons_enabled = false,
    theme = 'auto',
    component_separators = '|',
    section_separators = '',
  },
}

-- Enable Comment.nvim
require('Comment').setup()

-- Enable `lukas-reineke/indent-blankline.nvim`
-- See `:help indent_blankline.txt`
require('indent_blankline').setup {
  char = '┊',
  show_trailing_blankline_indent = false,
}

-- Gitsigns
-- See `:help gitsigns.txt`
require('gitsigns').setup {
  signs = {
    add = { text = '+' },
    change = { text = '~' },
    delete = { text = '_' },
    topdelete = { text = '‾' },
    changedelete = { text = '~' },
  },
}

-- [[ Configure Telescope ]]
-- See `:help telescope` and `:help telescope.setup()`
require('telescope').setup {
  defaults = {
    mappings = {
      i = {
        ['<C-u>'] = false,
        ['<C-d>'] = false,
      },
    },
  },
}

-- Enable telescope fzf native, if installed
pcall(require('telescope').load_extension, 'fzf')

require('treesitter').setup {
  ensure_installed = { 'c', 'cpp', 'go', 'lua', 'python', 'rust', 'typescript' }
}

require('lsp').setup {
  servers = {
    'clangd',
    'rust_analyzer',
    'pyright',
    'tsserver',
    'gopls',
    'solargraph',
    'terraformls',
    'omnisharp_mono'
  }
}

-- See `:help telescope.builtin`
vim.keymap.set('n', '<leader>?', require('telescope.builtin').oldfiles, { desc = '[?] Find recently opened files' })
vim.keymap.set('n', '<leader><space>', require('telescope.builtin').buffers, { desc = '[ ] Find existing buffers' })
vim.keymap.set('n', '<leader>/', function()
  -- You can pass additional configuration to telescope to change theme, layout, etc.
  require('telescope.builtin').current_buffer_fuzzy_find(require('telescope.themes').get_dropdown {
    winblend = 10,
    previewer = false,
  })
end, { desc = '[/] Fuzzily search in current buffer]' })

vim.keymap.set('n', '<leader>sf', require('telescope.builtin').find_files, { desc = '[S]earch [F]iles' })
vim.keymap.set('n', '<leader>sh', require('telescope.builtin').help_tags, { desc = '[S]earch [H]elp' })
vim.keymap.set('n', '<leader>sw', require('telescope.builtin').grep_string, { desc = '[S]earch current [W]ord' })
vim.keymap.set('n', '<leader>sg', require('telescope.builtin').live_grep, { desc = '[S]earch by [G]rep' })
vim.keymap.set('n', '<leader>sd', require('telescope.builtin').diagnostics, { desc = '[S]earch [D]iagnostics' })

-- Diagnostic keymaps
vim.keymap.set('n', '[d', vim.diagnostic.goto_prev)
vim.keymap.set('n', ']d', vim.diagnostic.goto_next)
vim.keymap.set('n', '<leader>d', vim.diagnostic.open_float)
vim.keymap.set('n', '<leader>q', vim.diagnostic.setloclist)

vim.keymap.set('n', 'tn', ':tabnew<cr>')
vim.keymap.set('n', 'tc', ':tabclose<cr>')
vim.keymap.set('n', 'tt', ':tabnext<cr>')
vim.keymap.set('n', 'tT', ':tabprevious<cr>')
vim.keymap.set('n', 'tm', ':tabmove ')
vim.keymap.set('n', 'tf', ':tabfirst<cr>')
vim.keymap.set('n', 'tl', ':tablast<cr>')
vim.keymap.set('n', 'to', ':tabonly<cr>')
vim.keymap.set('n', 'te', ':tabedit <c-r>=expand("%:p:h")<cr><cr>')
vim.keymap.set('n', 'tq', ':Tags<cr>')

vim.keymap.set('o', 'tn', 'tn')
vim.keymap.set('o', 'tc',  'tc')
vim.keymap.set('o', 'tt',  'tt')
vim.keymap.set('o', 'tT',  'tT')
vim.keymap.set('o', 'tp',  'tp')
vim.keymap.set('o', 'tm',  'tm')
vim.keymap.set('o', 'tf',  'tf')
vim.keymap.set('o', 'tl',  'tl')
vim.keymap.set('o', 'to',  'to')
vim.keymap.set('o', 'te',  'te')
vim.keymap.set('o', 'tq',  'tq')

vim.keymap.set('n', '<C-h>', '<C-w>h')
vim.keymap.set('n', '<C-l>', '<C-w>l')

vim.keymap.set('n', ',sp', ':sp<cr>')
vim.keymap.set('n', ',v', ':vsp<cr>')

vim.keymap.set('n', '<PageUp>', '<C-o>')
vim.keymap.set('n', '<PageDown>', '<C-i>')

vim.keymap.set('v', 'K', 'k')
vim.keymap.set('v', 'J', 'j')

-- Navigation
vim.keymap.set('n', "''v", ':e `=resolve(expand("~/.config/nvim/init.lua"))`<cr>')
vim.keymap.set('n', "''e", ':e <C-r>=resolve(expand("~/.config/nvim/lua"))<cr>/')
vim.keymap.set('n', "''x", ':e `=resolve(expand("~/.xmonad/xmonad.hs"))`<cr>')
vim.keymap.set('n', "''z", ':e `=resolve(expand("~/.zshrc"))`<cr>')
vim.keymap.set('n', "''l", ':e `=resolve(expand("~/.ledger"))`<cr>G')

vim.keymap.set('n', "'''", ':cd %:p:h<cr>')
vim.keymap.set('n', '\\e', ':e <C-r>=expand("%:h")<cr>/')
vim.keymap.set('n', '\\r', ':r <C-r>=expand("%:h")<cr>/')
vim.keymap.set('n', '\\w', ':w <C-r>=expand("%:h")<cr>/')

vim.keymap.set('v', '<', '<gv')
vim.keymap.set('v', '>', '>gv')

vim.keymap.set('n', 'gs', 'i<cr><esc>')

vim.keymap.set('c', '<C-a>', '<Home>')
vim.keymap.set('c', '<C-e>', '<End>')

vim.keymap.set('n', ',w', ':set list!<cr>')

vim.keymap.set('n', '<C-v>', 'v')
vim.keymap.set('n', 'v', '<C-v>')
vim.keymap.set('v', '<C-v>', 'v')

vim.keymap.set('n', ';', ':')
vim.keymap.set('n', 'q;', 'q:')

vim.api.nvim_create_user_command('Q', 'q', {bang = true})

-- Telescope
vim.keymap.set('n', '<C-p>', ':Telescope find_files<cr>')
vim.keymap.set('n', '<space>p', ':Telescope live_grep<cr>')
vim.keymap.set('n', 'tp', ':Telescope buffers<cr>')

vim.keymap.set('n', '<Tab>', ';')
vim.keymap.set('n', '<S-Tab>', ',')
vim.keymap.set('n', '<Tab>', '<Plug>Sneak_;')
vim.keymap.set('n', '<S-Tab>', '<Plug>Sneak_,')

vim.keymap.set('n', ',,', '<C-^>')

require('highlights')

vim.cmd('filetype indent off')
vim.cmd('set splitright splitbelow')

require('notes')

-- Github Copilot
vim.keymap.set('i', '<C-j>', 'copilot#Accept("")', {expr=true, silent=true, replace_keycodes=false})
vim.keymap.set('i', '<C-l>', '<Plug>(copilot-next)')
vim.keymap.set('i', '<C-h>', '<Plug>(copilot-next)')
