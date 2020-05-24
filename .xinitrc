xrdb ~/.Xresources
eval "$(set-xrdb-env)"
xsetroot -cursor_name left_ptr
setxkbmap us
setxkbmap -option compose:ralt

if [[ -e ~/.Xmodmap ]]; then
    xmodmap ~/.Xmodmap
fi

xrandr --output DP-1 --primary --auto
xrandr --output DP-1 --auto --left-of HDMI-0

/* feh --bg-scale ~/.background.png */
switch-dynamic-background
dunst-xrdb &
receive-remote-notifications &

systemctl --user import-environment DISPLAY XAUTHORITY

if command -v dbus-update-activation-environment >/dev/null 2>&1; then
    dbus-update-activation-environment DISPLAY XAUTHORITY
fi

eval $(/usr/bin/gnome-keyring-daemon --start --components=pkcs11,secrets,ssh)
export SSH_AUTH_SOCK

exec xmonad
