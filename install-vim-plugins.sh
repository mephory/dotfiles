#!/bin/zsh
while read p; do
    mkdir -p ~/.vim/bundle
    cd ~/.vim/bundle && git clone $p
    cd -
done < ./vim-plugins
