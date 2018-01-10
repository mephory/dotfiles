xrdb ~/.Xresources
xsetroot -cursor_name left_ptr
setxkbmap us
setxkbmap -option compose:ralt

if [[ -e ~/.Xmodmap ]]; then
    xmodmap ~/.Xmodmap
fi

synclient HorizEdgeScroll=0
synclient VertEdgeScroll=0
synclient HorizTwoFingerScroll=1
synclient VertTwoFingerScroll=1
synclient TapButton1=1
synclient TapButton2=2
synclient TapButton3=3

feh --bg-scale ~/.background.jpg
dunst &

exec xmonad
