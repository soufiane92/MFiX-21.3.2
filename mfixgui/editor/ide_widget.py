"""
Integrated Development Environment for MFiX.
"""

import os
import shutil

from qtpy.QtWidgets import (QWidget, QSplitter, QTabWidget, QHBoxLayout,
    QTabBar, QFileDialog, QShortcut, QMessageBox)
from qtpy.QtCore import Qt, Signal
from qtpy.QtGui import QKeySequence

from mfixgui.editor.tree import FileWidget
from mfixgui.editor.code_editor import CodeEditorWidget
from mfixgui.tools import get_mfix_src
from mfixgui.tools.qt import get_icon


class IDEWidget(QWidget):
    needs_saved_signal = Signal()
    file_changed = Signal(str)
    help_request = Signal() # Should we include the string?
    def __init__(self, parent=None):
        QWidget.__init__(self, parent)

        self.current_directory = '.'
        self.project_dir = None
        self.project_file = None

        layout = QHBoxLayout(self)
        layout.setContentsMargins(5, 5, 5, 5)

        splitter = QSplitter(self)
        layout.addWidget(splitter)

        # Tree
        self.tree = FileWidget(self)
        self.tree.mfix_source_file_clicked.connect(self.open_from_source)
        self.tree.project_file_clicked.connect(self.open)
        self.tree.new_file.connect(self.new_tab)
        splitter.addWidget(self.tree)

        # tab widget
        self.tabs = QTabWidget()
        # configure tab widget
        self.tabs.setTabsClosable(True)
        self.tabs.tabCloseRequested.connect(self.remove_tab)
        tab_bar = self.tabs.tabBar()
        tab_bar.setElideMode(Qt.ElideRight)

        # create a tab at the end to act like "new" button
        tab_bar.setSelectionBehaviorOnRemove(QTabBar.SelectLeftTab)
        new_tab = QWidget()
        self.tabs.addTab(new_tab, '')
        idx = self.tabs.count()-1
        self.tabs.setTabIcon(idx, get_icon('add.svg'))
        self.tabs.currentChanged.connect(self._tab_changed)
        # remove close btn
        right_btn = self.tabs.tabBar().tabButton(idx, QTabBar.RightSide)
        if right_btn:
            right_btn.resize(0, 0)
        left_btn = self.tabs.tabBar().tabButton(idx, QTabBar.LeftSide)
        if left_btn:
            left_btn.resize(0, 0)

        splitter.addWidget(self.tabs)

        splitter.setSizes([120, 600])

        # keyboard shortcuts
        shortcuts = [
            ('Ctrl+O', self.open),
            ('Ctrl+N', self.new_tab),
            ]

        for key, callback in shortcuts:
            q = QShortcut(QKeySequence(key), self)
            q.activated.connect(callback)

    def _tab_changed(self, index):
        if index == self.tabs.count()-1:
            # last tab was clicked, create new tab
            self.new_tab()

    def show_location(self, path, lineno):
        fullpath = os.path.join(get_mfix_src(), "model", path)
        self.open(fullpath, read_only=True).editor.goto(lineno)

    def open_from_source(self, fname, read_only=False):
        editor = self.open(fname, read_only=True)
        editor.copy_project_widget.setVisible(True)

    def copy_to_project(self, fname):
        # copy file to project directory
        if self.project_dir is not None:
            basename = os.path.basename(fname)
            cfilename = os.path.join(self.project_dir, basename)

            # check if file exists in project
            if os.path.exists(cfilename):
                rsp = QMessageBox.warning(
                    self, 'Overwrite file?',
                    'Warning: File exists in the project directory. Overwrite file?',
                    QMessageBox.Yes | QMessageBox.Cancel)

                if rsp == QMessageBox.Yes:
                    shutil.copy(fname, cfilename)
            else:
                shutil.copy(fname, cfilename)

            self.open(cfilename)
            open_index = self.is_open(fname)
            self.remove_tab(open_index)

    def open(self, fname=None, read_only=False, goto_tab=True, closeable=True,
             project_file=False):
        if fname is None:
            fname = QFileDialog.getOpenFileName(self)[0]
            if not fname:
                return

        if project_file:
            self.project_file = fname

        open_index = self.is_open(fname)
        if open_index > -1:
            if goto_tab:
                self.tabs.setCurrentIndex(open_index)
            editor = self.tabs.widget(open_index)
        else:
            tab_name = os.path.basename(fname)
            if read_only:
                tab_name += ' [read only]'
            editor = self.new_tab(fname=tab_name, closeable=closeable)
            editor.open(fname, read_only)
        return editor

    def open_project(self, prj):
        self.project_dir = prj
        self.tree.set_project_tree(prj)

    def new_tab(self, fname=None, closeable=True):
        w = CodeEditorWidget(self)
        index = self.tabs.count()-1
        w.editor.needsSaved.connect(lambda w=w: self.needs_saved(w))
        w.editor.help_request.connect(self.help_request.emit)
        w.editor.saved.connect(lambda w=w: self.handle_saved(w))
        self.tabs.insertTab(index, w, fname if fname is not None else 'untitled')
        self.tabs.setCurrentIndex(index)
        if not closeable:
            self.hide_close_button(index)
        return w

    def hide_close_button(self, idx=None):

        # remove close btn
        right_btn = self.tabs.tabBar().tabButton(idx, QTabBar.RightSide)
        if right_btn:
            right_btn.resize(0, 0)
        left_btn = self.tabs.tabBar().tabButton(idx, QTabBar.LeftSide)
        if left_btn:
            left_btn.resize(0, 0)

    def is_open(self, fname):
        """ see if fname is already open, return index else -1 """
        fname = os.path.abspath(fname)

        for index in range(self.tabs.count()):
            tab = self.tabs.widget(index)
            if hasattr(tab, 'editor') and tab.editor.file_name is not None and fname == os.path.abspath(tab.editor.file_name):
                return index
        return -1

    def remove_tab(self, index, check_needs_saved=True):
        """ handle signal to close tab """
        tab = self.tabs.widget(index)

        if check_needs_saved and tab.editor.unsaved_flag:
            message_box = QMessageBox(self)
            message_box.setWindowTitle("Save?")
            message_box.setIcon(QMessageBox.Warning)
            message_box.setText("Save changes before closing?")
            for b in [QMessageBox.Yes, QMessageBox.No, QMessageBox.Cancel]:
                message_box.addButton(b)
            message_box.setDefaultButton(QMessageBox.Cancel)
            ret = message_box.exec_()

            if ret == QMessageBox.Yes:
                success = tab.save()
                if not success:
                    return False
            elif ret == QMessageBox.Cancel:
                return False

        self.tabs.removeTab(index)
        return True

    def remove_untitled_tabs(self):
        for index in range(self.tabs.count()):
            tab = self.tabs.widget(index)
            if hasattr(tab, 'editor') and tab.editor.file_name is None:
                self.remove_tab(index, check_needs_saved=False)

    def reset(self):
        for i in range(self.tabs.count()-1):
            self.remove_tab(0)

    def needs_saved(self, tab):
        index = self.tabs.indexOf(tab)
        t = self.tabs.tabText(index)
        if not t.startswith('*'):
            self.tabs.setTabText(index, '*' + t)
        self.needs_saved_signal.emit()

    def check_needs_saved(self):
        for index in range(self.tabs.count()):
            tab = self.tabs.widget(index)
            if hasattr(tab, 'editor'):
                if tab.editor.unsaved_flag:
                    return True
        return False

    def handle_saved(self, tab):
        index = self.tabs.indexOf(tab)
        self.tabs.setTabText(index, os.path.basename(tab.editor.file_name))
        self.file_changed.emit(tab.editor.file_name)
