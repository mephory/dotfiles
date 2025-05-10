-- Notes
vim.keymap.set('n', ',n', ':Telescope find_files cwd=~/Documents/Notes<cr>')
vim.keymap.set('n', ',N', ':Telescope live_grep cwd=~/Documents/Notes<cr>')
vim.keymap.set('n', ',e', ':e ~/Documents/Notes/')
vim.keymap.set('n', ',w', ':w ~/Documents/Notes/')
vim.keymap.set('n', ',m', ':Mkdir ~/Documents/Notes/')
vim.keymap.set('n', ',d', ':e ~/Documents/Notes/daily/<c-r>=strftime("%Y-%m-%d")<cr>.md<cr>')
vim.keymap.set('n', ',g', ':e ~/Documents/Notes/todo.md<cr>')
vim.keymap.set('n', ',f', ':e %:h/<cfile><cr>')

vim.keymap.set('i', '<F2>', '<c-r>=strftime("%Y-%m-%d %H:%M")<cr>')
vim.keymap.set('i', '<F3>', '<c-r>=strftime("%Y-%m-%d")<cr>')
vim.keymap.set('i', '<F4>', '<c-r>=strftime("%H:%M")<cr>')

-- vim.keymap.set('n', ',1', function() dailyNote(0) end)
-- vim.keymap.set('n', ',2', function() dailyNote(1) end)
-- vim.keymap.set('n', ',3', function() dailyNote(2) end)
-- vim.keymap.set('n', ',4', function() dailyNote(3) end)
-- vim.keymap.set('n', ',5', function() dailyNote(4) end)
-- vim.keymap.set('n', ',6', function() dailyNote(5) end)

function dailyNote(daysAgo)
    local prevDay = vim.fn.strftime("%Y-%m-%d", vim.fn.localtime() - daysAgo * 86400)
    vim.cmd.edit("~/Documents/Notes/daily/" .. prevDay .. ".md")
end
