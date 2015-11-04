xrdb ~/.Xresources
# xmodmap ~/.Xmodmap
xsetroot -cursor_name left_ptr

setxkbmap -option compose:ralt
setxkbmap -option 'caps:ctrl_modifier'
xcape -e 'Caps_Lock=Escape;Control_L=Escape;Control_R=Escape'

synclient HorizEdgeScroll=0
synclient VertEdgeScroll=0
synclient HorizTwoFingerScroll=1
synclient VertTwoFingerScroll=1
synclient TapButton1=1
synclient TapButton2=2
synclient TapButton3=3

feh --bg-scale ~/.background.png
dunst &
# compton &
wm restartall

exec xmonad
wm stopall
