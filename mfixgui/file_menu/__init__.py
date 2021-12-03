"""the file menu bar"""
from os.path import join

from qtpy.QtCore import QPoint, QPropertyAnimation, QSize, Qt
from qtpy.QtWidgets import (QGridLayout,  QLabel,  QListWidget,
                            QListWidgetItem,  QSizePolicy, QStackedWidget,
                            QToolButton, QWidget)

from mfixgui.animations import animation_speed
from mfixgui.bug_report import save_bug_report
from mfixgui.tools.qt import sub_icon_size, set_alignment

# This code is split into lots and lots of little files.
from mfixgui.file_menu.about_widget import AboutWidget
from mfixgui.file_menu.export_widget import ExportWidget
from mfixgui.file_menu.help_widget import HelpWidget
from mfixgui.file_menu.info_widget import InfoWidget
from mfixgui.file_menu.new_project_widget import NewProjectWidget
from mfixgui.file_menu.open_widget import OpenWidget
from mfixgui.file_menu.save_as_widget import SaveAsWidget
from mfixgui.file_menu.new_mesh_widget import NewMeshWidget
from mfixgui.file_menu.settings_widget import SettingsWidget
from mfixgui.tools.qt import get_icon, get_pixmap, get_separator, SETTINGS
from mfixgui.widgets.nodeworks import NODEWORKS_AVAILABLE


class MenuItem(QListWidgetItem):
    def __init__(self, name, icon=None, callback=None, ui_item=None):
        if icon is not None:
            icon = get_icon(icon)
            super(MenuItem, self).__init__(icon, name)
        else:
            super(MenuItem, self).__init__(name)
        if callback is not None:
            self.callback = callback
        elif ui_item is not None:
            ui, item = ui_item
            self.callback = lambda ui=ui,item=item: ui.stackedwidget_file_menu.setCurrentWidget(item)
        else:
            raise ValueError("Either callback, or ui & item is required")

    def select(self):
        # There is no reason for this abstraction - if the item is disabled, the click
        # event won't come through.  No need for this extra 'enabled' check - cgw
        if self.enabled():
            self.callback()

    def set_enabled(self, is_enabled):
        if is_enabled:
            self.setFlags(self.flags() | Qt.ItemIsEnabled)
        else:
            self.setFlags(self.flags() & ~Qt.ItemIsEnabled)

    def enabled(self):
        return bool(self.flags() & Qt.ItemIsEnabled)


class FileMenu:
    """File menu mixin for the gui."""

    def init_file_menu(self, mfixgui):
        """Build the file menu."""
        ui = self.ui
        self.gui = mfixgui
        self.file_menu = QWidget(self)
        self.file_menu.setObjectName("file_menu")
        self.file_menu.setStyleSheet("QWidget#file_menu{background-color: #E0E0E0;}")
        self.file_menu.hide()

        ui.file_menu_back = self.make_back_button()
        menu = ui.file_menu_list = make_list_widget()
        ui.stackedwidget_file_menu = QStackedWidget()

        self.add_items()

        fm = menu.fontMetrics()
        w = (max(fm.boundingRect(menu.item(i).text()).width()
                 for i in range(menu.count()))  # Widest string in menu
             + sub_icon_size().width()          # Room for icons if used
             + 30)                              # Margin
        menu.setMaximumWidth(w)

        # Should just use designer instead of doing all of this layout
        layout = ui.file_menu_layout = QGridLayout(self.file_menu)
        layout.setContentsMargins(0, 0, 0, 0)

        layout.addWidget(ui.file_menu_back, 0, 0)
        layout.addWidget(ui.file_menu_list, 1, 0)
        layout.addWidget(make_logo_label(), 2, 0)
        layout.setRowStretch(1, 0)
        layout.addWidget(ui.stackedwidget_file_menu, 0, 1, -1, 1)

    def add_items(self):
        """ Add MenuItems to the QListWidget, Widgets to the QStackedWidget, and connect them """
        ui = self.ui
        self.info_widget = InfoWidget(self.gui)
        self.new_project_widget = NewProjectWidget(self.gui)
        self.new_mesh_widget = NewMeshWidget(self.gui)
        self.open_widget = OpenWidget(self.gui)
        self.save_as_widget = SaveAsWidget(self.gui)
        self.export_widget = ExportWidget(self.gui)

        self.about_widget = AboutWidget()
        self.help_widget = HelpWidget(self.gui)
        self.settings_widget = SettingsWidget(self.gui)

        stacked = ui.stackedwidget_file_menu
        stacked.setStyleSheet(""".QWidget{background-color: white;}
        QStackedWidget{background-color: white;}""")
        stacked.addWidget(self.info_widget)
        stacked.addWidget(self.new_project_widget)
        stacked.addWidget(self.open_widget)
        stacked.addWidget(self.new_mesh_widget)

        stacked.addWidget(self.save_as_widget)
        stacked.addWidget(self.export_widget)
        stacked.addWidget(self.settings_widget)
        stacked.addWidget(self.help_widget)
        stacked.addWidget(self.about_widget)

        ui.export_project = MenuItem( "Export project", 'open_in_new', self.show_export)
        ui.export_workflow = MenuItem("Export workflow", 'open_in_new', self.nodeworks_export)
        ui.import_workflow = MenuItem("Import workflow", 'import', self.nodeworks_import)
        ui.new_project = MenuItem("New project", 'newfolder', self.switch_to_new)
        ui.open_project = MenuItem("Open project", 'openfolder', ui_item=(ui, self.open_widget))
        ui.new_mesh = MenuItem("New mesh", 'newfolder', self.new_mesh)
        ui.new_mesh.setHidden(True) # Beta feature, must be enabled
        ui.open_mesh = MenuItem("Open mesh", 'openfolder', self.open_mesh)
        ui.open_mesh.setHidden(True) # Beta feature, must be enabled
        ui.info_item = MenuItem("Project info", 'info', ui_item=(ui, self.info_widget))
        ui.save = MenuItem("Save project", 'save', self.handle_save)
        ui.save_as = MenuItem("Save to new name", 'save', self.show_save_as)
        ui.bug_report = MenuItem("Submit bug report", 'bug', self.submit_bug_report)
        file_items = [ui.info_item,
                      ui.new_project,
                      ui.open_project,
                      ui.new_mesh,
                      ui.open_mesh,
                      ui.save,
                      ui.save_as,
                      ui.export_project]

        nodeworks_items = [ui.import_workflow, ui.export_workflow]
        app_items = [MenuItem("Settings", 'settings', ui_item=(ui, self.settings_widget)),
                     MenuItem("Help", 'help', ui_item=(ui, self.help_widget)),
                     MenuItem("About MFiX", 'info', ui_item=(ui, self.about_widget)),
                     MenuItem("Quit", 'close', self.close)]

        for file_item in file_items:
            ui.file_menu_list.addItem(file_item)
        ui.file_menu_list.addItem(ui.bug_report)
        if NODEWORKS_AVAILABLE:
            self.add_separator()
            for nodeworks_item in nodeworks_items:
                ui.file_menu_list.addItem(nodeworks_item)
        self.add_separator()
        for app_item in app_items:
            ui.file_menu_list.addItem(app_item)

    def new_mesh(self, *args):
        self.new_mesh_widget.show()
        self.ui.stackedwidget_file_menu.setCurrentWidget(self.new_mesh_widget)

    def open_mesh(self, *args):
        self.gui.error("Not implemented", popup=True)

    def show_save_as(self):
        self.save_as_widget.show()
        self.ui.stackedwidget_file_menu.setCurrentWidget(self.save_as_widget)

    def show_export(self):
        self.export_widget.show()
        self.ui.stackedwidget_file_menu.setCurrentWidget(self.export_widget)

    def submit_bug_report(self):
        save_bug_report(self.gui)

    def make_back_button(self):
        btn = QToolButton(self.file_menu)
        btn.setIcon(get_icon("left.svg"))
        btn.setText("Back")
        btn.setToolButtonStyle(Qt.ToolButtonTextBesideIcon)
        btn.setAutoRaise(True)
        btn.clicked.connect(self.hide_file_menu)
        btn.setSizePolicy(QSizePolicy(QSizePolicy.Minimum, QSizePolicy.Minimum))
        return btn

    def handle_open_shortcut(self):
        """handle Ctrl+O shortcut"""
        self.open_file_menu()
        lw = self.ui.file_menu_list
        lw.setCurrentItem(self.ui.open_project)
        self.ui.stackedwidget_file_menu.setCurrentWidget(self.open_widget)

    def handle_new_shortcut(self):
        """handle Ctrl+N shortcut"""
        self.open_file_menu()
        lw = self.ui.file_menu_list
        lw.setCurrentItem(self.ui.new_project)
        self.switch_to_new()

    def nodeworks_export(self):
        self.ui.nodeworks_widget.handle_export()

    def nodeworks_import(self):
        self.ui.nodeworks_widget.handle_import()

    def switch_to_new(self):
        self.ui.stackedwidget_file_menu.setCurrentWidget(self.new_project_widget)
        self.new_project_widget.search_bar.setFocus()

    def open_file_menu(self, item=None):
        """Update and show the file menu,  display menu item if specified"""
        ui = self.ui
        if item is None:
            if self.get_project_file() is not None:
                ui.file_menu_list.setCurrentItem(ui.info_item)
                ui.stackedwidget_file_menu.setCurrentWidget(self.info_widget)
            elif SETTINGS.value("recent_projects", ""):
                ui.file_menu_list.setCurrentItem(ui.open_project)
                ui.stackedwidget_file_menu.setCurrentWidget(self.open_widget)
            else:
                ui.file_menu_list.setCurrentItem(ui.new_project)
                ui.stackedwidget_file_menu.setCurrentWidget(self.new_project_widget)

        self.enable_menu_items()
        self.settings_widget.update() # Reflect any updated settings
        self.info_widget.update_info() # 1061

        w = ui.toolbutton_menu.width()
        h = ui.toolbutton_menu.height()
        ui.file_menu_back.setMinimumWidth(w)
        ui.file_menu_back.setMinimumHeight(h)

        # hide the current widget, issue #291, VTK widget overlay issues
        ui.tabWidgetGraphics.currentWidget().hide()

        if item:
            items = ui.file_menu_list.findItems(item, Qt.MatchFixedString)
            if len(items) == 1:
                ui.file_menu_list.setCurrentItem(items[0])
            else:
                raise ValueError(item)
            widget = getattr(self, item + '_widget', None)
            widget.show()
            self.ui.stackedwidget_file_menu.setCurrentWidget(widget)

        if not self.file_menu.isVisible():
            self.show_file_menu()

    def enable_menu_items(self):
        project_loaded = self.get_project_file() is not None
        self.ui.file_menu_back.setEnabled(project_loaded)
        self.ui.info_item.set_enabled(project_loaded)
        self.ui.save.set_enabled(project_loaded and self.gui.unsaved_flag)
        self.ui.save_as.set_enabled(project_loaded)
        self.ui.export_project.set_enabled(project_loaded)
        if NODEWORKS_AVAILABLE:
            self.ui.export_workflow.set_enabled(project_loaded)
            self.ui.import_workflow.set_enabled(project_loaded)

    def show_file_menu(self):
        """Animate showing the file menu"""
        w, h = self.width(), self.height()
        self.file_menu.setGeometry(-w/2 , 0, w, h) # Why w/2 here?
        self.file_menu.show()
        self.file_menu.raise_()
        ani = make_animation(self.file_menu, (-w/4, 0), (0, 0)) # w/4 ?
        self.file_menu_animation = ani
        ani.finished.connect(self.finish_show_file_menu)
        ani.start()

    def hide_file_menu(self):
        """Animate hiding the file menu"""
        w = self.width()
        ani = make_animation(self.file_menu, (0, 0), (-w/4, 0))
        self.file_menu_animation = ani
        ani.finished.connect(self.finish_hide_file_menu)
        ani.start()

    def finish_show_file_menu(self):
        """callback when the show animation is finished"""
        w, h = self.width(), self.height()
        self.file_menu.setGeometry(0, 0, w, h)
        self.file_menu.show()
        self.file_menu.raise_()

    def finish_hide_file_menu(self):
        """callback when the hide animation is finished"""
        # show the current widget, issue #291, VTK widget overlay issues
        self.ui.tabWidgetGraphics.currentWidget().show()
        self.file_menu.hide()

    def check_unsaved_abort(self):
        """ Popup confirmation dialog if project is unsaved """
        if self.unsaved_flag:
            confirm = self.message(
                text="Project not saved\nData may be lost!\nProceed?",
                buttons=["ok", "cancel"],
                default="cancel")
            if confirm != "ok":
                return True
            self.clear_unsaved_flag()
        return False

    def update_info(self):
        self.info_widget.update_info()

    def project_notes(self):
        return self.info_widget.project_notes()

    def add_separator(self):
        list_item = QListWidgetItem()
        list_item.select = lambda: None
        list_item.setFlags(Qt.NoItemFlags)
        list_item.setSizeHint(QSize(0, 10))
        self.ui.file_menu_list.addItem(list_item)
        sep = get_separator(vertical=False)
        sep.setEnabled(False)
        self.ui.file_menu_list.setItemWidget(list_item, sep)
        return list_item

    def handle_update(self):
        """ Switch to About menu item to show that MFiX Update is available """
        self.open_file_menu()
        self.about_widget.show()
        self.ui.stackedwidget_file_menu.setCurrentWidget(self.about_widget)

    def show_update_available(self, latest_version):
        self.about_widget.show_update_available(latest_version)


def make_list_widget():
    lw = QListWidget()
    lw.setFrameStyle(lw.NoFrame)
    lw.setSizePolicy(
        QSizePolicy(QSizePolicy.MinimumExpanding, QSizePolicy.MinimumExpanding)
    )
    lw.setAttribute(Qt.WA_MacShowFocusRect, 0)
    lw.setStyleSheet(
        """QListView{background-color: #E0E0E0;}
                        QListView::item:selected{background: #64B5F6;
                            color: white;}
                        QListView::item:hover{background:#BBDEFB;}
                      """
    )
    lw.itemClicked.connect(lambda item: item.select())
    return lw


def make_logo_label():
    l = QLabel()
    pixmap = get_pixmap("mfix.png", 84, 84)  # Why 84x84?
    l.setPixmap(pixmap)
    set_alignment(l, Qt.AlignCenter)
    return l


def make_animation(target, start, end):
    (x_start, y_start) = start
    (x_end, y_end) = end
    animation = QPropertyAnimation(target, b"pos")
    animation.setDuration(animation_speed())
    animation.setStartValue(QPoint(x_start, y_start))
    animation.setEndValue(QPoint(x_end, y_end))
    return animation
