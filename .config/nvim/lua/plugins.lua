return {
  { -- LSP
    'neovim/nvim-lspconfig',
    dependencies = {
      'williamboman/mason.nvim',
      'williamboman/mason-lspconfig.nvim',
      { 'j-hui/fidget.nvim', branch = 'legacy' }
    },
  },

  { -- Autocompletion
    'hrsh7th/nvim-cmp',
    dependencies = { 'hrsh7th/cmp-nvim-lsp', 'L3MON4D3/LuaSnip', 'saadparwaiz1/cmp_luasnip' },
  },
  { 'hrsh7th/cmp-nvim-lsp-signature-help' },

  { -- Treesitter
    'nvim-treesitter/nvim-treesitter',
    build = function()
      pcall(require('nvim-treesitter.install').update { with_sync = true })
    end,
    dependencies = { 'nvim-treesitter/nvim-treesitter-textobjects' }
  },

  { 'tommcdo/vim-lion' },
  { 'tpope/vim-unimpaired' },
  { 'tpope/vim-eunuch' },
  { 'tpope/vim-surround' },
  { 'tpope/vim-repeat' },
  { 'tpope/vim-fugitive' },
  { 'lewis6991/gitsigns.nvim' },
  { 'justinmk/vim-sneak' },
  { 'catppuccin/nvim' },
  { 'github/copilot.vim' },
  -- { "supermaven-inc/supermaven-nvim" },
  { 'nvim-lualine/lualine.nvim' }, -- Fancier statusline
  { 'numToStr/Comment.nvim' }, -- "gc" to comment visual regions/lines
  { 'rrethy/vim-hexokinase', build = 'make hexokinase' },

  { 'tpope/vim-sleuth' }, -- Detect tabstop and shiftwidth automatically
  { 'lukas-reineke/indent-blankline.nvim' }, -- Add indentation guides even on blank lines

  -- Telescope
  { 'nvim-telescope/telescope.nvim', branch = '0.1.x', dependencies = { 'nvim-lua/plenary.nvim' } },
  { 'nvim-telescope/telescope-fzf-native.nvim', build = 'make', cond = vim.fn.executable 'make' == 1 },

  -- Follow system theme
  {
    "f-person/auto-dark-mode.nvim",
    opts = {
      update_interval = 1000,
      set_dark_mode = function()
        vim.api.nvim_set_option("background", "dark")
        vim.cmd("hi Normal guibg=NONE ctermbg=NONE")
      end,
      set_light_mode = function()
        vim.api.nvim_set_option("background", "light")
        vim.cmd("hi Normal guibg=NONE ctermbg=NONE")
      end,
    }
  },

  {
    "joshuavial/aider.nvim",
    opts = {
      auto_manage_context = true, -- automatically manage buffer context
      default_bindings = true,    -- use default <leader>A keybindings
      debug = false,              -- enable debug logging
    },
  }
}
