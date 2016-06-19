#!/bin/sh
set -e

name="Gauche-${GAUCHE_VERSION}"
source="$name.tgz"
wget -O "$source" "http://prdownloads.sourceforge.net/gauche/$source"
tar xzvf "$source"
(cd $name; ./configure --prefix=$HOME; make; make install)
