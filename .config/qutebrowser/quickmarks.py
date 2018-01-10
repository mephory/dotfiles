quickmarks = {
    "t": "https://twitter.com/",
    "m": "https://www.fastmail.com/",
    "f": "https://facebook.com/",
    "u": "http://upload.mephory.com/",
    "s": "http://smashboards.com/",
    "x": "http://xkcd.com/",
    "y": "https://www.youtube.com/",
    "l": "http://www.last.fm/user/mephory",
    "n": "https://news.ycombinator.com/",
    "j": "http://stevinho.justnetwork.eu/",
    "a": "http://mephory.com:3000/",
    "d": "http://dota.ninja/",
    "g": "https://github.com/",
}

def register_quickmarks(config):
    config.unbind('go')

    for key, url in quickmarks.items():
        config.bind("go{}".format(key), "open {}".format(url))
        config.bind("gn{}".format(key), "open -t {}".format(url))
