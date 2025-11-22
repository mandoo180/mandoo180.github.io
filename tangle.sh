#!/bin/bash
#
# Tangle all Emacs configuration files
#

echo "Tangling all configuration files..."
if [ ! -d ~/.emacs.d/elisp ]; then
	echo "Elisp directory does not exist."
	echo 'Making ~/.emacs.d/elisp/settings...'
	mkdir -p ~/.emacs.d/elisp/settings
	echo 'Making ~/.emacs.d/elisp/themes...'
	mkdir -p ~/.emacs.d/elisp/themes
fi
emacs --batch --load tangle-configs.el

echo "Done! Configuration files have been generated in ~/.emacs.d/"
