xrdb ~/.Xresources
# xmodmap ~/.Xmodmap
xsetroot -cursor_name left_ptr
setxkbmap -option compose:caps

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

# UIM Setup
export GTK_IM_MODULE='xim'
export QT_IM_MODULE='xim'
uim-xim &
export XMODIFIERS='@im=uim'

exec xmonad
wm stopall
