xrdb ~/.Xresources
eval "$(set-xrdb-env)"
xsetroot -cursor_name left_ptr
setxkbmap us
setxkbmap -option compose:ralt

if [[ -e ~/.Xmodmap ]]; then
    xmodmap ~/.Xmodmap
fi

# xrandr --output HDMI-0 --auto --left-of DP-2 --scale-from 2560x1440 --pos 0x0
xrandr --output DP-0 --auto --primary --pos 2560x0
xrandr --output DP-2 --auto --left-of DP-0 --pos 0x0
# xrandr --output DP-5 --right-of DP-0 --auto

feh --bg-scale ~/.background.png
dunst-xrdb &
receive-remote-notifications &

systemctl --user import-environment DISPLAY XAUTHORITY

if command -v dbus-update-activation-environment >/dev/null 2>&1; then
    dbus-update-activation-environment DISPLAY XAUTHORITY
fi

eval $(/usr/bin/gnome-keyring-daemon --start --components=pkcs11,secrets,ssh)
export SSH_AUTH_SOCK

export GTK_IM_MODULE='ibus'
export QT_IM_MODULE='ibus'
export XMODIFIERS='@im=ibus'
ibus-daemon -drx

exec xmonad
