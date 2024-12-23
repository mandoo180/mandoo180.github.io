#!/bin/bash

emacs -Q --script build-site.el
sudo cp ./public/* /var/www/html/
