#!/bin/sh
PACKAGES="$(cat arch-packages | grep -v '^#' | grep -vE '^\s+$' | tr "\n" " ")"
sudo pacman -S $PACKAGES
