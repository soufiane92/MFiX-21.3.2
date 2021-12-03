#!/bin/bash
# Convenience script for developers: run the mfix gui from current directory, without installing

HERE=$(dirname "$0")
SOURCE_DIR=$(cd $HERE; cd ..; pwd)

# Work around Nodewords import error when starting in widgets subdir
if [[ $PWD == $SOURCE_DIR/mfixgui/widgets ]] ; then
    cd ..
fi

PYTHONPATH="$SOURCE_DIR:$PYTHONPATH" python -m mfixgui.gui -d "$@"
