"""
File widget consisting of two tree widgets for browsing the MFiX source files
and project files.
"""
import os
from shutil import copyfile

from qtpy.QtWidgets import (QTreeView, QFileSystemModel, QWidget, QHBoxLayout,
    QSplitter, QVBoxLayout, QLabel, QLineEdit, QToolButton, QFileDialog,
    QGridLayout, QMenu, QAction, QMessageBox, QApplication)
from qtpy.QtCore import Qt, Signal
from qtpy.QtGui import QClipboard

from mfixgui.tools import get_mfix_src
from mfixgui.tools.qt import get_icon
from mfixgui.editor.constants import LANGUAGE_DEFAULTS


FILE_EXTS = []
for lang_ext in LANGUAGE_DEFAULTS.values():
    FILE_EXTS.extend(['*.' + ext for ext in lang_ext['exts']])


class FileWidget(QWidget):
    mfix_source_file_clicked = Signal(str)
    project_file_clicked = Signal(str)
    new_file = Signal()
    def __init__(self, parent=None):
        QWidget.__init__(self, parent)
        self.ide_widget = parent
        layout = QHBoxLayout(self)
        layout.setContentsMargins(0, 0, 0, 0)
        splitter = QSplitter(Qt.Vertical, self)
        layout.addWidget(splitter)

        def make_search(tree, place_holder_text):
            search = QLineEdit()
            search.setPlaceholderText(place_holder_text)
            search.textChanged.connect(lambda text: tree.set_filter(text))
            return search

        # project dir
        prj_widget = QWidget(self)
        splitter.addWidget(prj_widget)
        prj_layout = QGridLayout (prj_widget)
        prj_layout.setContentsMargins(0, 0, 0, 0)
        prj_widget.setLayout(prj_layout)

        prj_label = QLabel("Project Files")
        prj_source = self.project_tree = FileTree(self)
        prj_source.clicked.connect(lambda i, t=prj_source:self.click_event(i, t))
        self.open_project_button = QToolButton(self)
        self.open_project_button.setIcon(get_icon('openfolder'))
        self.open_project_button.clicked.connect(lambda *args: self.open_project())
        self.open_project_button.setAutoRaise(True)
        self.open_project_button.setToolTip('Open project')

        prj_layout.addWidget(prj_label, 0, 0)
        prj_layout.addWidget(self.open_project_button, 0, 1)
        prj_layout.addWidget(make_search(prj_source, 'Search project'), 1, 0, 1, 2)
        prj_layout.addWidget(prj_source, 2, 0, 1, 2)

        # mfix source
        src_widget = QWidget(self)
        splitter.addWidget(src_widget)
        src_layout = QVBoxLayout(src_widget)
        src_layout.setContentsMargins(0, 0, 0, 0)
        src_widget.setLayout(src_layout)

        src_label = QLabel('MFiX Source')
        mfix_source = FileTree(self, root_path=os.path.join(get_mfix_src(), 'model'), prj_dir=False)
        mfix_source.clicked.connect(lambda i, t=mfix_source:self.click_event(i, t, True))

        src_layout.addWidget(src_label)
        src_layout.addWidget(make_search(mfix_source, 'Search source'))
        src_layout.addWidget(mfix_source)

    def click_event(self, index, tree, source=False):
        item = tree.model.fileInfo(index)
        if item.exists() and item.isFile():
            if source:
                self.mfix_source_file_clicked.emit(item.absoluteFilePath())
            else:
                self.project_file_clicked.emit(item.absoluteFilePath())

    def set_project_tree(self, prj):
        self.project_tree.set_root_path(prj)

    def open_project(self, prj_dir=None):
        if prj_dir is None:
            prj_dir = QFileDialog.getExistingDirectory(self)
            if not prj_dir:
                return
        self.set_project_tree(prj_dir)

    def close_file(self, fname, check_needs_saved=True):

        index = self.ide_widget.is_open(fname)
        if index >= 0:
            return self.ide_widget.remove_tab(index, check_needs_saved)
        return True

class FileTree(QTreeView):
    def __init__(self, parent=None, root_path=None, prj_dir=True):
        QTreeView.__init__(self, parent)
        self.file_widget = parent
        self.model = QFileSystemModel()
        self.setModel(self.model)
        self.root_path = None
        if root_path is not None:
            self.set_root_path(root_path)

        self.model.setNameFilterDisables(False)
        self.set_filter()

        # hide all columns except for the first one
        [self.hideColumn(i) for i in range(1, self.model.columnCount())]
        self.setHeaderHidden(True)

        self.setContextMenuPolicy(Qt.CustomContextMenu)
        self.customContextMenuRequested.connect(self.show_context_menu)

        # context menu
        self.menu_index = None
        self.menu = QMenu(self)

        menu_items = []
        if prj_dir:
            menu_items += [('Browse', self.browse),
                           ('New File', self.new),
                           ('Rename', self.rename),
                           ('Duplicate', self.duplicate),
                           ('Delete', self.delete),
                           ]
        menu_items += [('Copy Full Path', self.copy_path)]

        self.menu_actions = []
        for label, slot in menu_items:
            a = QAction(label, self.menu)
            a.triggered.connect(slot)
            self.menu_actions.append(a)
            self.menu.addAction(a)

    def set_root_path(self, root_path):
        self.root_path = root_path = os.path.abspath(root_path)
        self.model.setRootPath(root_path)
        self.setRootIndex(self.model.index(root_path))

    def set_filter(self, text=None):
        if text is None or not text:
            self.model.setNameFilters(FILE_EXTS)
            self.collapseAll()
        else:
            filters = ['*' + text + ext for ext in FILE_EXTS]
            self.model.setNameFilters(filters)
            self.expandAll()

    def show_context_menu(self, point):
        self.menu_index = self.indexAt(point)

        valid = self.menu_index.isValid()
        project_file = os.fspath(self.get_index_path()) == os.fspath(self.file_widget.ide_widget.project_file)
        # disable actions if not a file
        is_file = valid and os.path.isfile(self.get_index_path())
        for i in range(len(self.menu_actions)):
            self.menu_actions[i].setEnabled((is_file and not project_file) or i in [0, 1, 5])

        self.menu.exec_(self.viewport().mapToGlobal(point))

    def get_index_path(self):
        return self.model.fileInfo(self.menu_index).absoluteFilePath()

    def browse(self):
        path = self.get_index_path()
        if os.path.isfile(path):
            path = os.path.dirname(path)
        fname = QFileDialog.getOpenFileName(
                    self, "Open a file", path)

        if isinstance(fname, tuple):
            fname = fname[0]

        if not fname:
            return

        if path not in os.path.abspath(fname):
            reply = QMessageBox.question(self, "Copy", "Copy file to project directory?\n{}".format(path),
                                    QMessageBox.Yes|QMessageBox.No)
            if reply == QMessageBox.Yes:
                new_fname = os.path.join(path, os.path.basename(fname))
                copyfile(fname, new_fname)
                fname = new_fname

        self.file_widget.project_file_clicked.emit(fname)

    def new(self):
        self.file_widget.new_file.emit()

    def rename(self):
        path = self.get_index_path()
        fname = QFileDialog.getSaveFileName(
                    self, "Rename file", path)

        if isinstance(fname, tuple):
            fname = fname[0]

        if not fname:
            return
        success = self.file_widget.close_file(path)
        if success:
            os.replace(path, fname)

    def duplicate(self):
        path = self.get_index_path()
        fname = QFileDialog.getSaveFileName(
                    self, "Duplicate file", path)

        if isinstance(fname, tuple):
            fname = fname[0]

        if not fname or fname == path:
            return

        # duplicate
        copyfile(path, fname)

    def delete(self):
        path = self.get_index_path()

        reply = QMessageBox.question(self, "Delete", "Are you sure you want to delete:\n{}".format(path),
                                QMessageBox.Yes|QMessageBox.No)
        if reply == QMessageBox.Yes:
            self.file_widget.close_file(path, check_needs_saved=False)
            os.remove(path)

    def copy_path(self):
        path = self.get_index_path()
        app = QApplication.instance()
        app.clipboard().setText(path)
