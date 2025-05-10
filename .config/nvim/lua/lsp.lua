local M = {}

function M.setup(user_data)
  local on_attach = function(args)
    local nmap = function(keys, func)
      vim.keymap.set('n', keys, func, { buffer = args.buf })
    end

    local imap = function(keys, func)
      vim.keymap.set('i', keys, func, { buffer = args.buf })
    end

    nmap('<leader>rn', vim.lsp.buf.rename)
    nmap('<leader>ca', vim.lsp.buf.code_action)

    nmap('gd', vim.lsp.buf.definition)
    nmap('gr', require('telescope.builtin').lsp_references)
    nmap('gI', vim.lsp.buf.implementation)
    nmap('<leader>D', vim.lsp.buf.type_definition)
    nmap('<leader>ls', require('telescope.builtin').lsp_document_symbols)
    nmap('<leader>la', require('telescope.builtin').lsp_dynamic_workspace_symbols)

    -- See `:help K` for why this keymap
    nmap('K', vim.lsp.buf.hover)
    nmap('<C-k>', vim.lsp.buf.signature_help)
    imap('<C-k>', vim.lsp.buf.signature_help)

    -- Lesser used LSP functionality
    nmap('gD', vim.lsp.buf.declaration)

    -- Create a command `:Format` local to the LSP buffer
    vim.api.nvim_buf_create_user_command(args.buf, 'Format', function(_)
      if vim.lsp.buf.format then
        vim.lsp.buf.format()
      elseif vim.lsp.buf.formatting then
        vim.lsp.buf.formatting()
      end
    end, { desc = 'Format current buffer with LSP' })
  end

  -- Setup mason so it can manage external tooling
  require('mason').setup()

  -- Ensure the servers above are installed
  require('mason-lspconfig').setup {
    ensure_installed = user_data.servers,
  }

  -- Turn on lsp status information
  require('fidget').setup()

  -- nvim-cmp setup
  local cmp = require 'cmp'
  local luasnip = require 'luasnip'

  cmp.setup {
    snippet = {
      expand = function(args)
        luasnip.lsp_expand(args.body)
      end,
    },
    mapping = cmp.mapping.preset.insert {
      ['<C-d>'] = cmp.mapping.scroll_docs(-4),
      ['<C-f>'] = cmp.mapping.scroll_docs(4),
      ['<C-Space>'] = cmp.mapping.complete(),
      ['<C-k>'] = cmp.mapping.confirm {
        behavior = cmp.ConfirmBehavior.Replace,
        select = true,
      },
      ['<Tab>'] = cmp.mapping(function(fallback)
        if cmp.visible() then
          cmp.select_next_item()
        elseif luasnip.expand_or_jumpable() then
          luasnip.expand_or_jump()
        else
          fallback()
        end
      end, { 'i', 's' }),
      ['<S-Tab>'] = cmp.mapping(function(fallback)
        if cmp.visible() then
          cmp.select_prev_item()
        elseif luasnip.jumpable(-1) then
          luasnip.jump(-1)
        else
          fallback()
        end
      end, { 'i', 's' }),
    },
    sources = {
      { name = 'nvim_lsp' },
      { name = 'luasnip' },
      { name = 'nvim_lsp_signature_help' }
    },
  }

  vim.api.nvim_create_autocmd("LspAttach", {
    group = vim.api.nvim_create_augroup("my.lsp", {}),
    callback = on_attach,
  })
end

return M
