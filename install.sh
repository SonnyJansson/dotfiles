#!/bin/sh

cd $(dirname $0)
D="$(pwd)"

# Doom Emacs
ln -sv "$D/.doom.d" ~

# i3
ln -sv "$D/.i3" ~

# Fonts
ln -sv "$D/.fonts" ~

# Scripts
ln -sv "$D/.scripts" ~

# Wallpaper

ln -sv "$D/.wallpaper" ~

# .config files
mkdir -p ~/.config
ln -sv "$D/.config/polybar" ~/.config
ln -sv "$D/.config/ranger" ~/.config
# ln -sv ./.config

# Miscellaneous
ln -sv "$D/misc/.vimrc" ~
ln -sv "$D/misc/.xinitrc" ~
ln -sv "$D/misc/.Xresources" ~
ln -sv "$D/misc/.zshrc" ~
