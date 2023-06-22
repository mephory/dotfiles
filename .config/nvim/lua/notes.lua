-- Notes
vim.keymap.set('n', '<space>n', ':Telescope find_files cwd=~/data/notes<cr>')
vim.keymap.set('n', '<space>N', ':Telescope live_grep cwd=~/data/notes<cr>')
vim.keymap.set('n', '<space>e', ':e ~/data/notes/')
vim.keymap.set('n', '<space>w', ':w ~/data/notes/')
vim.keymap.set('n', '<space>m', ':Mkdir ~/data/notes/')
vim.keymap.set('n', '<space>d', ':e ~/data/notes/daily/<c-r>=strftime("%Y-%m-%d")<cr>.md<cr>')
vim.keymap.set('n', '<space>g', ':e ~/data/notes/todo.md<cr>')
vim.keymap.set('n', '<space>f', ':e %:h/<cfile><cr>')

vim.keymap.set('i', '<F2>', '<c-r>=strftime("%Y-%m-%d %H:%M")<cr>')
vim.keymap.set('i', '<F3>', '<c-r>=strftime("%Y-%m-%d")<cr>')
vim.keymap.set('i', '<F4>', '<c-r>=strftime("%H:%M")<cr>')

-- local notes_group = vim.api.nvim_create_augroup('Notes', { clear = true })
-- vim.api.nvim_create_autocmd('BufWritePost', {
--   command = 'silent! !notes postprocess',
--   group = notes_group,
--   pattern = '/home/mephory/data/notes/*.md'
-- })
--
-- function NotesPreWrite()
--     local line, col = unpack(vim.api.nvim_win_get_cursor(0))
--     vim.api.nvim_command("silent! %!notes preprocess")
--     vim.api.nvim_win_set_cursor(0, { line, col })
-- end
--
-- vim.api.nvim_create_autocmd('BufWritePre', {
--   -- command = 'silent! %!~/data/notes/.prewrite.sh',
--   callback = NotesPreWrite,
--   group = notes_group,
--   pattern = '/home/mephory/data/notes/*.md',
-- })
--
