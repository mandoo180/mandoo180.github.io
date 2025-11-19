#!/bin/bash
#
# Tangle all Emacs configuration files
#

echo "Tangling all configuration files..."
emacs --batch --load tangle-configs.el

echo "Done! Configuration files have been generated in ~/.emacs.d/"
