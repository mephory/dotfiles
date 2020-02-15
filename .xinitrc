xrdb ~/.Xresources
xsetroot -cursor_name left_ptr
setxkbmap us
setxkbmap -option compose:ralt

if [[ -e ~/.Xmodmap ]]; then
    xmodmap ~/.Xmodmap
fi

xrandr --output DP-1 --primary --auto
xrandr --output DP-1 --auto --left-of HDMI-0

feh --bg-scale ~/.background.png
dunst &
receive-remote-notifications &
polybar default &

exec xmonad
