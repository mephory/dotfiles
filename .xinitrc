xrdb ~/.Xresources
xsetroot -cursor_name left_ptr
setxkbmap us
setxkbmap -option compose:ralt

if [[ -e ~/.Xmodmap ]]; then
    xmodmap ~/.Xmodmap
fi

# xrandr --output HDMI-0 --auto --left-of DP-2 --scale-from 2560x1440 --pos 0x0
xrandr \
  --fb 7440x2560 \
  --output DP-0 --auto --primary --pos 2560x560 --mode 3440x1440 --rate 143.92 \
  --output DP-2 --auto --pos 6000x0 --rotate right \
  --output DP-4 --auto --pos 0x560

if [[ -f ~/.background.1.jpg ]]; then
  feh --bg-fill ~/.background.2.jpg ~/.background.3.jpg ~/.background.1.jpg
else
  feh --bg-fill ~/.background.jpg
fi

receive-remote-notifications &

systemctl --user import-environment DISPLAY XAUTHORITY

if command -v dbus-update-activation-environment >/dev/null 2>&1; then
    dbus-update-activation-environment DISPLAY XAUTHORITY
fi

eval $(/usr/bin/gnome-keyring-daemon --start --components=pkcs11,secrets,ssh)
/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1 &
export SSH_AUTH_SOCK

export GLFW_IM_MODULE='ibus'
export GTK_IM_MODULE='ibus'
export QT_IM_MODULE='ibus'
export XMODIFIERS='@im=ibus'
ibus-daemon -drx

# pywal -R -s -t

exec xmonad
