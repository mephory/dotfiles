-- Keymaps for better default experience
-- See `:help vim.keymap.set()`
vim.keymap.set({ 'n', 'v' }, '<Space>', '<Nop>', { silent = true })

-- Other
vim.keymap.set('v', 'K', 'k')
vim.keymap.set('v', 'J', 'j')
vim.keymap.set('v', '<', '<gv')
vim.keymap.set('v', '>', '>gv')
vim.keymap.set('c', '<C-a>', '<Home>')
vim.keymap.set('c', '<C-e>', '<End>')
vim.keymap.set('n', ';', ':')
vim.keymap.set('n', 'q;', 'q:')
vim.keymap.set('n', ',,', '<C-^>')
vim.keymap.set('n', '<space>e', ':silent! .w !bash<cr>')
vim.api.nvim_create_user_command('Q', 'q', {bang = true})

-- Diagnostic keymaps
vim.keymap.set('n', '[d', vim.diagnostic.goto_prev)
vim.keymap.set('n', ']d', vim.diagnostic.goto_next)
vim.keymap.set('n', '<leader>d', vim.diagnostic.open_float)
vim.keymap.set('n', '<leader>q', vim.diagnostic.setloclist)

-- Tabs
vim.keymap.set('n', 'tn', ':tabnew<cr>')
vim.keymap.set('n', 'tc', ':tabclose<cr>')
vim.keymap.set('n', 'tt', ':tabnext<cr>')
vim.keymap.set('n', 'tT', ':tabprevious<cr>')
vim.keymap.set('n', 'tm', ':tabmove ')
vim.keymap.set('n', 'tf', ':tabfirst<cr>')
vim.keymap.set('n', 'tl', ':tablast<cr>')
vim.keymap.set('n', 'to', ':tabonly<cr>')
vim.keymap.set('o', 'tn', 'tn')
vim.keymap.set('o', 'tc',  'tc')
vim.keymap.set('o', 'tt',  'tt')
vim.keymap.set('o', 'tT',  'tT')
vim.keymap.set('o', 'tp',  'tp')
vim.keymap.set('o', 'tm',  'tm')
vim.keymap.set('o', 'tf',  'tf')
vim.keymap.set('o', 'tl',  'tl')
vim.keymap.set('o', 'to',  'to')

-- Window Navigation
vim.keymap.set('n', '<C-h>', '<C-w>h')
vim.keymap.set('n', '<C-l>', '<C-w>l')

-- Splits
vim.keymap.set('n', ',s', ':sp<cr>')
vim.keymap.set('n', ',v', ':vsp<cr>')

-- Navigate jumplist with page up and page down
vim.keymap.set('n', '<PageUp>', '<C-o>')
vim.keymap.set('n', '<PageDown>', '<C-i>')

-- Quick open often used files
vim.keymap.set('n', "''v", ':e `=resolve(expand("~/.config/nvim/init.lua"))`<cr>')
vim.keymap.set('n', "''e", ':e <C-r>=resolve(expand("~/.config/nvim/lua"))<cr>/<C-d>')
vim.keymap.set('n', "''s", ':e `=resolve(expand("~/.skhdrc"))`<cr>')
vim.keymap.set('n', "''y", ':e `=resolve(expand("~/.yabairc"))`<cr>G')
vim.keymap.set('n', "''z", ':e `=resolve(expand("~/.zshrc"))`<cr>')

-- :e, :r, :w relative to current file
vim.keymap.set('n', '\\e', ':e <C-r>=expand("%:h")<cr>/')
vim.keymap.set('n', '\\r', ':r <C-r>=expand("%:h")<cr>/')
vim.keymap.set('n', '\\w', ':w <C-r>=expand("%:h")<cr>/')

vim.keymap.set('n', 'gs', 'i<cr><esc>')

-- Swap v and <C-v>
vim.keymap.set('n', '<C-v>', 'v')
vim.keymap.set('n', 'v', '<C-v>')
vim.keymap.set('v', '<C-v>', 'v')

-- Swap ; and , with Tab and S-Tab
vim.keymap.set('n', '<Tab>', '<Plug>Sneak_;')
vim.keymap.set('n', '<S-Tab>', '<Plug>Sneak_,')

-- Github Copilot
vim.keymap.set('i', '<C-j>', 'copilot#Accept("")', {expr=true, silent=true, replace_keycodes=false})
vim.keymap.set('i', '<C-l>', '<Plug>(copilot-next)')
vim.keymap.set('i', '<C-h>', '<Plug>(copilot-next)')

-- Telescope
vim.keymap.set('n', '<C-p>', require('telescope.builtin').find_files)
vim.keymap.set('n', '<space>p', require('telescope.builtin').live_grep)
vim.keymap.set('n', 'tp', require('telescope.builtin').buffers)
vim.keymap.set('n', '<leader><space>', require('telescope.builtin').buffers)
vim.keymap.set('n', '<leader>?', require('telescope.builtin').oldfiles)
vim.keymap.set('n', '<leader>j', require('telescope.builtin').jumplist)
vim.keymap.set('n', '<leader>/', function()
  require('telescope.builtin').current_buffer_fuzzy_find(require('telescope.themes').get_dropdown {
    winblend = 10,
    previewer = false,
  })
end)


-- vim.keymap.set('n', '<space>d', ':exec &bg=="light"? "set bg=dark" : "set bg=light"<CR>', {noremap = true, silent = true})
