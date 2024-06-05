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


-- Indentation settings
vim.o.tabstop = 4
vim.o.shiftwidth = 4
vim.o.shiftround = true
vim.o.breakindent = true
vim.o.autoindent = true
vim.cmd [[filetype indent off]]

-- UI settings
vim.o.hlsearch = false
vim.o.mouse = ''
vim.o.showbreak = '«'
vim.wo.number = true
vim.wo.signcolumn = 'yes'
vim.o.termguicolors = true
vim.cmd [[colorscheme catppuccin-mocha]]
vim.cmd [[set splitright splitbelow]]

-- Other
vim.o.undofile = true
vim.o.ignorecase = true
vim.o.smartcase = true
vim.o.updatetime = 250
vim.o.completeopt = 'menuone,noselect'
vim.filetype.add({ extension = { templ = "templ" } })

-- Quirk for editing sql files without annoying autocomplete on pressing tab
vim.g.ftplugin_sql_omni_key = '<C-j>'


-- Load in custom modules
require('keymaps')
require('highlights')
require('notes')
require('treesitter').setup {
  ensure_installed = {
    'c',
    'cpp',
    'go',
    'lua',
    'python',
    'rust',
    'typescript'
  }
}
require('lsp').setup {
  servers = {
    'templ',
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

-- Configure plugins
require('lualine').setup {
  options = {
    icons_enabled = true,
    theme = 'auto',
    component_separators = '|',
    section_separators = '',
  },
}

require('Comment').setup()

require('ibl').setup {  -- lukas-reineke/indent-blankline.nvim
  scope = {
    enabled = false
  }
}

require('gitsigns').setup {
  signs = {
    add = { text = '+' },
    change = { text = '~' },
    delete = { text = '_' },
    topdelete = { text = '‾' },
    changedelete = { text = '~' },
  },
}

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
pcall(require('telescope').load_extension, 'fzf')

local highlight_group = vim.api.nvim_create_augroup('YankHighlight', { clear = true })
vim.api.nvim_create_autocmd('TextYankPost', {
  callback = function()
    vim.highlight.on_yank()
  end,
  group = highlight_group,
  pattern = '*',
})
