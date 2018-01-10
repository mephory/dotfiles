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

c.colors.tabs.odd.bg = '#000000'
c.colors.tabs.even.bg = '#000000'
c.colors.tabs.odd.fg = '#ffffff'
c.colors.tabs.even.fg = '#ffffff'
c.colors.tabs.selected.odd.bg = '#ffffff'
c.colors.tabs.selected.even.bg = '#ffffff'
c.colors.tabs.selected.odd.fg = '#000000'
c.colors.tabs.selected.even.fg = '#000000'

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

quickmarks.register_quickmarks(config)
