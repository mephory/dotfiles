vim.g.netrw_banner = 0
vim.g.netrw_browsex_viewers = 'xdg-open'

vim.g.table_mode_corner_corner = '+'
vim.g.table_mode_header_fillchar = '='
vim.g.table_mode_toggle_map = 'q'

vim.g['pandoc#hypertext#open_editable_alternates'] = 1
vim.g['pandoc#toc#close_after_navigating'] = 0

vim.g.typescript_indent_disable = 1

vim.g.gruvbox_contrast_dark = 'hard'
vim.g.gruvbox_transparent_bg = '1'

require('telescope').setup{
  defaults = {
    vimgrep_arguments = {
      "rga",
      "--color=never",
      "--no-heading",
      "--with-filename",
      "--line-number",
      "--column",
      "--smart-case"
    }
  }
}
