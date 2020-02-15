import quickmarks

c.url.start_pages = 'file:///home/mephory/data/homepage/index.html'
c.url.default_page = 'file:///home/mephory/data/homepage/index.html'
c.hints.mode = 'number'
c.hints.auto_follow = 'always'
c.tabs.background = True
c.tabs.padding = { "top": 7, "bottom": 7, "left": 7, "right": 7 }
c.tabs.favicons.scale = 1.5
c.url.searchengines = { "DEFAULT": "https://google.com/search?q={}" }
c.backend = "webengine"
c.fonts.default_family = "meslo"

c.colors.tabs.odd.bg = '#002b36'
c.colors.tabs.even.bg = '#002b36'
c.colors.tabs.odd.fg = '#fdf6e3'
c.colors.tabs.even.fg = '#fdf6e3'
c.colors.tabs.selected.odd.bg = '#fdf6e3'
c.colors.tabs.selected.even.bg = '#fdf6e3'
c.colors.tabs.selected.odd.fg = '#002b36'
c.colors.tabs.selected.even.fg = '#002b36'
c.colors.completion.even.bg = '#002b36'
c.colors.completion.odd.bg = '#073642'
c.colors.completion.item.selected.bg = '#268bd2'
c.colors.completion.item.selected.fg = '#eee8d5'
c.colors.completion.item.selected.border.bottom = '#268bd2'
c.colors.completion.item.selected.border.top = '#268bd2'
c.colors.completion.fg = '#eee8d5'
c.colors.completion.match.fg = '#d33682'
c.colors.completion.category.bg = 'qlineargradient(x1:0, y1:0, x2:0, y2:1, stop:0 #073642, stop:1 #002b36)'

config.bind('t', 'set-cmd-text -s :open -t')
config.bind('T', 'set-cmd-text :open -t {url}')
config.bind('O', 'set-cmd-text :open {url}')
config.bind('gh', 'tab-prev')
config.bind('gl', 'tab-next')
config.bind('gr', 'tab-prev')
config.bind('gt', 'tab-focus')
config.bind('gH', 'tab-move -')
config.bind('gL', 'tab-move +')
config.bind('<Ctrl-Tab>', 'tab-next')
config.bind('<Ctrl-Shift-Tab>', 'tab-prev')
config.bind('gi', 'hint inputs ;; later 50 follow-hint 0')
config.bind(';', 'set-cmd-text :')
config.bind(',m', 'set-cmd-text -s :tab-move')
config.bind(',b', 'set-cmd-text -s :buffer')
config.bind(',,', 'tab-focus last')
config.bind(',.', 'config-source')
config.bind('<escape>', 'leave-mode ;; jseval -q document.activeElement.blur()', mode='insert')
config.bind(',q', 'set-cmd-text -s :quickmark-load')
config.bind(',Q', 'set-cmd-text -s :quickmark-load -t')
config.bind('<Ctrl-e>', 'scroll down')
config.bind('<Ctrl-y>', 'scroll up')
config.bind('zo', 'tab-give')

quickmarks.register_quickmarks(config)
