require 'paq' {
  'dhruvasagar/vim-table-mode',
  'justinmk/vim-sneak',
  'ludovicchabant/vim-gutentags',
  'romainl/vim-qf',
  'tommcdo/vim-exchange',
  'tommcdo/vim-lion',
  'tpope/vim-abolish',
  'tpope/vim-commentary',
  'tpope/vim-eunuch',
  'tpope/vim-fugitive',
  'tpope/vim-surround',
  'tpope/vim-unimpaired',
  'tpope/vim-rbenv',
  'tpope/vim-rails',
  'tpope/vim-bundler',
  'vim-pandoc/vim-pandoc',
  'vim-pandoc/vim-pandoc-syntax',
  'wellle/targets.vim',
  'xolox/vim-misc',
  'nvim-lua/plenary.nvim',
  'nvim-telescope/telescope.nvim',
  {'nvim-treesitter/nvim-treesitter', run=TSUpdate},
  'neovim/nvim-lspconfig',

  'dylanaraps/wal.vim',
  'arcticicestudio/nord-vim',
  'morhetz/gruvbox.vim',
  'Mofiqul/dracula.vim',

  'leafgarland/typescript-vim',
  'peitalin/vim-jsx-typescript',
}

require('config.base')
require('config.plugins')
require('config.treesitter')
require('config.maps')
require('config.notes')
require('config.lsp')

-- local paste_snippet = function()
--   local start = vim.fn.getpos("'<")
--   local end = vim.fn.getpos("'>")
--   vim.api.nvim_buf_get_lines(0, h
--   print("TEST")
-- end

-- vim.keymap.set('n', '<leader>p', paste_snippet)
