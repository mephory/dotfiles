xrdb ~/.Xresources
# xmodmap ~/.Xmodmap
xsetroot -cursor_name left_ptr
setxkbmap us
setxkbmap -option compose:ralt

synclient HorizEdgeScroll=0
synclient VertEdgeScroll=0
synclient HorizTwoFingerScroll=1
synclient VertTwoFingerScroll=1
synclient TapButton1=1
synclient TapButton2=2
synclient TapButton3=3

feh --bg-scale ~/.background.jpg
dunst &
xcompmgr &
# compton &
wm restartall

xmodmap -e "clear Lock"
xmodmap -e "keycode 66 = Mode_switch NoSymbol Mode_switch NoSymbol Mode_switch Mode_switch"
xmodmap -e "keycode 43 = h H Left"
xmodmap -e "keycode 44 = j J Down"
xmodmap -e "keycode 45 = k K Up"
xmodmap -e "keycode 46 = l L Right"
xmodmap -e "keycode 31 = i I Home"
xmodmap -e "keycode 57 = n N End"
xmodmap -e "keycode 30 = u U Prior"
xmodmap -e "keycode 32 = o O Next"
xmodmap -e "keycode 10 = 1 exclam F1"
xmodmap -e "keycode 11 = 2 at F2"
xmodmap -e "keycode 12 = 3 numbersign F3"
xmodmap -e "keycode 13 = 4 dollar F4"
xmodmap -e "keycode 14 = 5 percent F5"
xmodmap -e "keycode 15 = 6 asciicircum F6"
xmodmap -e "keycode 16 = 7 ampersand F7"
xmodmap -e "keycode 17 = 8 asterisk F8"
xmodmap -e "keycode 18 = 9 parenleft F9"
xmodmap -e "keycode 19 = 0 parenright F10"
xmodmap -e "keycode 47 = semicolon colon Insert"

exec xmonad
wm stopall
