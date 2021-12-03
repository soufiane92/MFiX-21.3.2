"""
Entry point for the MFiX Editor.
"""

from qtpy.QtWidgets import QApplication
import argparse
import sys
import os

from mfixgui.editor.ide_widget import IDEWidget
from mfixgui.tools.qt import get_icon

def main():

    parser = argparse.ArgumentParser(description='MFiX GUI editor')
    ARG = parser.add_argument
    ARG('file', action='store', nargs='*', default=None,
        help='open a file.')
    ARG('-p', '--project', action='store', default=None,
        help='open a project.')
    args = parser.parse_args(sys.argv[1:])

    app = QApplication([])
    editor = IDEWidget()
    editor.setGeometry(50, 50, 800, 800)
    editor.setWindowIcon(get_icon('file_edit'))
    editor.setWindowTitle('MFiX Editor')
    editor.show()
    if args.file:
        for f in args.file:
            if os.path.exists(f) and os.path.isfile(f):
                editor.open(f)
            else:
                print('File does not exists: {}'.format(f))
    else:
        editor.new_tab()

    prj = args.project
    if prj is not None:
        if os.path.exists(prj) and os.path.isdir(prj):
            editor.open_project(prj)
        else:
            print('Project does not exists: {}'.format(prj))

    app.exec_()


if __name__ == "__main__":
    main()
