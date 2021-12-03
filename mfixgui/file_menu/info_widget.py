import json

from PyQt5.QtWidgets import QWidget

from mfixgui.tools.qt import get_ui


class InfoWidget(QWidget):
    def __init__(self, gui):
        super(InfoWidget, self).__init__()
        get_ui("info.ui", widget=self)
        self.gui = gui
        self.project_notes_textedit.undoAvailable.connect(gui.set_unsaved_flag)
        self.update_info()

    def project_notes(self):
        """ Return the text of the Project Notes text area """
        return self.project_notes_textedit.toPlainText()

    def update_info(self):
        """ Update info fields """
        project = self.gui.project
        comments = project.mfix_gui_comments
        self.filename.setText(self.gui.get_project_file() or "Unknown")
        self.created_by.setText(comments.get("author", ""))
        self.created_time.setText(comments.get("created_date", ""))
        self.gui_version.setText(str(comments.get("gui_version", "")))
        self.modified_by.setText(comments.get("modified_by", "").replace("|", ", "))
        self.modified_time.setText(comments.get("modified_time", ""))
        self.project_notes_textedit.setText(json.loads(comments.get("project_notes", '""')))
        self.project_version.setText(str(comments.get("project_version", "")))
