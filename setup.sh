#!/bin/bash

DOTFILES_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
EMACS_D="$HOME/.emacs.d/"

echo "Vim:"
# TODO: Install nvim
if [ ! -f "$HOME/.vimrc" ]; then
  echo "  Installing .vimrc"
  ln -s "$DOTFILES_DIR/.vimrc" "$HOME/.vimrc"
else
  echo "  Skipping .vimrc"
fi

printf "\nEmacs:"
# TODO: Install emacs (at the latest version)
if [ ! -d "$EMACS_D" ]; then
  echo "  Creating $EMACS_D"
  mkdir "$EMACS_D"
fi

# Symlink init.el
echo
if [ ! -f "$EMACS_D/init.el" ]; then
  echo "  Installing init.el"
  ln -s "$DOTFILES_DIR/emacs/init.el" "$EMACS_D/init.el"
else
  echo "  Skipping init.el"
fi

# Symlink config/
if [ ! -d "$EMACS_D/config/" ]; then
  echo "  Installing config"
  ln -s "$DOTFILES_DIR/emacs/config/" "$EMACS_D/config"
else
  echo "  Skipping config"
fi
