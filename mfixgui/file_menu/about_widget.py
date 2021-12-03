import os

from PyQt5.QtCore import QUrl, QSize
from PyQt5.QtGui import QTextCursor, QDesktopServices
from PyQt5.QtWidgets import QApplication, QSizePolicy, QWidget

from mfixgui.tools.qt import get_ui, get_icon

from mfixgui.version import __version__
from mfixgui.version_info import get_version_info


class AboutWidget(QWidget):
    """ Display version information for MFiX and its dependencies """

    def __init__(self):
        """ Create widget for About filemenu pane """
        super(AboutWidget, self).__init__()
        get_ui("about.ui", widget=self)
        self.about_label.setSizePolicy(
            QSizePolicy(QSizePolicy.Minimum, QSizePolicy.Maximum)
        )
        self.copy_btn.clicked.connect(self.copy_version_info)
        self.copy_btn.setIcon(get_icon('copy.svg'))

        def open_url(url):
            return QDesktopServices.openUrl(QUrl(url))

        self.supportforum_lbl.linkActivated.connect(open_url)
        self.update_notification_lbl.linkActivated.connect(open_url)
        version_txt = os.linesep.join(get_version_info())
        self.version_info_txtbrowser.setText(version_txt)

        size = self.supportforum_lbl.sizeHint().height()
        self.copy_btn.setIconSize(QSize(size, size))
        self.update_notification_lbl.setVisible(False)

    def copy_version_info(self):
        """ copy contents of TextBrowser to clipboard """
        txt = self.version_info_txtbrowser.toPlainText()
        QApplication.instance().clipboard().setText(txt)
        self.select_text(txt)

    def select_text(self, txt):
        """ for visual feedback that the text was copied """
        cursor = self.version_info_txtbrowser.textCursor()
        cursor.setPosition(0)
        cursor.setPosition(len(txt), QTextCursor.KeepAnchor)
        self.version_info_txtbrowser.setTextCursor(cursor)

    def show_update_available(self, latest_version):
        self.update_notification_lbl.setText(
            f"You are running MFiX {__version__}. "
            f"Version {latest_version} is now available for download from "
            f'<a href="https://mfix.netl.doe.gov">the MFiX website</a>'
        )
        self.update_notification_lbl.setVisible(True)
