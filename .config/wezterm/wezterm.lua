local wezterm = require 'wezterm';

return {
  automatically_reload_config = true,
  use_ime = false,
  enable_tab_bar = false,
  -- color_scheme = "Gruvbox Dark",
  font = wezterm.font("JetBrains Mono"),
  font_size = 14,
  exit_behavior = "Close",
  keys = {
    {key="g", mods="CTRL|SHIFT", action="QuickSelect"},
    {key="d", mods="CTRL|SHIFT", action="QuickSelect"}
  },
  colors = {
    foreground = os.getenv('WISP_FOREGROUND'),
    background = os.getenv('WISP_BACKGROUND'),
    cursor_bg = os.getenv('WISP_FOREGROUND'),
    cursor_fg = os.getenv('WISP_BACKGROUND'),
    cursor_border = os.getenv('WISP_FOREGROUND'),
    selection_fg = os.getenv('WISP_BACKGROUND'),
    selection_bg = os.getenv('WISP_FOREGROUND'),
    scrollbar_thumb = "#222222",
    split = "#444444",
    ansi = {
      os.getenv('WISP_COLOR0'),
      os.getenv('WISP_COLOR1'),
      os.getenv('WISP_COLOR2'),
      os.getenv('WISP_COLOR3'),
      os.getenv('WISP_COLOR4'),
      os.getenv('WISP_COLOR5'),
      os.getenv('WISP_COLOR6'),
      os.getenv('WISP_COLOR7'),
    },
    brights = {
      os.getenv('WISP_COLOR8'),
      os.getenv('WISP_COLOR9'),
      os.getenv('WISP_COLOR10'),
      os.getenv('WISP_COLOR11'),
      os.getenv('WISP_COLOR12'),
      os.getenv('WISP_COLOR13'),
      os.getenv('WISP_COLOR14'),
      os.getenv('WISP_COLOR15'),
    }
  }
}
