#!/bin/zsh
DOTFILES=$(
    echo .{*(.),*/**/*(.)} |
        tr " " "\n" |
        grep -v "^\.git/" |
        grep -v "^\.$" |
        grep -v "^\.\.$"
)

echo 'This will install the dotfiles to ~/.'
echo 'These are the files to be installed:'
echo "$DOTFILES"
echo ''
echo 'Do you want to install them now? [y/N]'

read I
if [[ $I != "y" ]]; then
    echo "Nothing changed."
    exit;
fi;

while read f; do
    if [ -e "$HOME/$f" ]; then
        echo "$HOME/$f does already exist. Backing up to $HOME/$f.bak"
        # mv "$HOME/$f" "$HOME/$f.bak"
        rm "$HOME/$f"
    fi
    mkdir -p "$(dirname $HOME/$f)"
    echo "ln -s $PWD/$f $HOME/$f"
    ln -s "$PWD/$f" "$HOME/$f"
done < <(echo $DOTFILES)

echo "all done."
