vim.g.mapleader = ','

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

vim.keymap.set('n', '<BS>', '<C-w>h')
vim.keymap.set('n', '<C-h>', '<C-w>h')
vim.keymap.set('n', '<C-l>', '<C-w>l')

-- Splits
vim.keymap.set('n', '<leader>sp', ':sp<cr>')
vim.keymap.set('n', '<leader>v', ':vsp<cr>')

vim.keymap.set('n', '<leader>b', ':ls<cr>:b ')
vim.keymap.set('n', '<leader>,', '<C-^>')

vim.keymap.set('n', '<Tab>', ';')
vim.keymap.set('n', '<S-Tab>', ',')
vim.keymap.set('n', '<Tab>', '<Plug>Sneak_;')
vim.keymap.set('n', '<S-Tab>', '<Plug>Sneak_,')

vim.keymap.set('n', '<PageUp>', '<C-o>')
vim.keymap.set('n', '<PageDown>', '<C-i>')

vim.keymap.set('v', 'K', 'k')
vim.keymap.set('v', 'J', 'j')

vim.keymap.set('n', 'zT', 'zt10<C-y>')
vim.keymap.set('n', 'zB', 'zb10<C-e>')


-- Navigation
vim.keymap.set('n', "''v", ':e `=resolve(expand("~/.config/nvim/init.lua"))`<cr>')
vim.keymap.set('n', "''b", ':e `=resolve(expand("~/.config/nvim/lua/config/base.lua"))`<cr>')
vim.keymap.set('n', "''m", ':e `=resolve(expand("~/.config/nvim/lua/config/maps.lua"))`<cr>')
vim.keymap.set('n', "''n", ':e `=resolve(expand("~/.config/nvim/lua/config/notes.lua"))`<cr>')
vim.keymap.set('n', "''l", ':e `=resolve(expand("~/.config/nvim/lua/config/lsp.lua"))`<cr>')
vim.keymap.set('n', "''x", ':e `=resolve(expand("~/.xmonad/xmonad.hs"))`<cr>')
vim.keymap.set('n', "''z", ':e `=resolve(expand("~/.zshrc"))`<cr>')

vim.keymap.set('n', "'''", ':cd %:p:h<cr>')
vim.keymap.set('n', '\\e', ':e <C-r>=expand("%:h")<cr>/')
vim.keymap.set('n', '\\r', ':r <C-r>=expand("%:h")<cr>/')
vim.keymap.set('n', '\\w', ':w <C-r>=expand("%:h")<cr>/')

vim.keymap.set('n', '<leader>h', 'yypv$r-k')
vim.keymap.set('n', '<leader>H', 'yypv$r=k')

vim.keymap.set('v', '<', '<gv')
vim.keymap.set('v', '>', '>gv')

vim.keymap.set('n', '<leader>tm', ':TableModeToggle<cr>')
vim.keymap.set('v', '<leader>so', ':sort<cr>')

vim.keymap.set('n', '<leader>c', 'gcc')
vim.keymap.set('v', '<leader>c', 'gc')

vim.keymap.set('n', 'gs', 'i<cr><esc>')

vim.keymap.set('n', 'Y', 'y$')

vim.keymap.set('c', '<C-a>', '<Home>')
vim.keymap.set('c', '<C-e>', '<End>')

vim.keymap.set('n', 'v', '<C-v>')
vim.keymap.set('n', '<C-v>', 'v')
vim.keymap.set('v', 'v', '<C-v>')
vim.keymap.set('v', '<C-v>', 'v')

vim.keymap.set('n', '<leader>w', ':set list!<cr>')
vim.keymap.set('n', ';', ':')
vim.keymap.set('n', 'q;', 'q:')

vim.keymap.set('v', '.', ':norm.<cr>')

vim.keymap.set('n', '<leader>.', ':source ~/.config/nvim/init.lua<cr>')

vim.api.nvim_create_user_command('Q', 'q', {bang = true})

vim.keymap.set('v', '++', 'VMATH_YankAndAnalyse()', { expr = true })
vim.keymap.set('n', '++', 'vip++', { noremap = false })

-- Telescope
vim.keymap.set('n', '<space>n', ':Telescope find_files cwd=~/data/obsidian/default<cr>')
vim.keymap.set('n', '<space>N', ':Telescope live_grep cwd=~/data/obsidian/default<cr>')
vim.keymap.set('n', '<C-p>', ':Telescope find_files<cr>')
vim.keymap.set('n', '<space>p', ':Telescope live_grep<cr>')
vim.keymap.set('n', 'tp', ':Telescope buffers<cr>')


vim.keymap.set('n', '<space>g', ':lua vim.o.lines = 15<cr>:lua vim.o.columns = 80<cr>')
