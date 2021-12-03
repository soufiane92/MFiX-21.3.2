import os
import glob
from qtpy import QtCore, QtGui, QtWidgets
import ffmpeg
import re
from vtk.util.numpy_support import vtk_to_numpy
import numpy as np

from mfixgui.tools.qt import get_icon, sub_icon_size, get_ui, SETTINGS


FFMPEG_TIME_RX = re.compile(b'time=(\d{2}):(\d{2}):(\d{2})\.\d{2}')
FFMPEG_DURATION_RX = re.compile(b'Duration: (\d{2}):(\d{2}):(\d{2})\.\d{2}')


def seconds(hours, minutes, seconds):
    return (int(hours) * 60 + int(minutes)) * 60 + int(seconds)


class CreateMoviePopUp(QtWidgets.QDialog):

    def __init__(self, parent=None):
        QtWidgets.QDialog.__init__(self, parent)
        self.parent = parent
        self.gui = parent.gui
        self.ffmpeg_process = None
        self.duration = None

        ui = self.ui = get_ui('create_movie.ui', self)

        # override sizeHint
        self.ui.sizeHint = self.size_hint

        # connect
        ui.toolButton_browse_path.setIcon(get_icon('folder.svg'))
        ui.toolButton_browse_path.clicked.connect(self.browse)
        ui.toolButton_browse_image_stack.setIcon(get_icon('folder.svg'))
        ui.toolButton_browse_image_stack.clicked.connect(self.browse_image_stack)
        ui.pushButton_create.clicked.connect(self.create_movie)
        ui.pushButton_close.clicked.connect(self.done)

        ui.groupBox_use_image_stack.toggled.connect(self.enable_size_widgets)

        ui.combobox_template.currentIndexChanged.connect(self.change_size)

        ui.lineedit_width.dtype = int
        ui.lineedit_height.dtype = int

        if not ui.lineEdit_path.text():
            ui.lineEdit_path.setText(self.gui.get_project_dir())

        # hide resolution widgets
        if not int(SETTINGS.value('enable_screenshot_res', 0)):
            for wid in [ui.label_template, ui.combobox_template,
                        ui.label_width, ui.lineedit_width,
                        ui.label_height, ui.lineedit_height]:
                wid.setVisible(False)

        self.progress_timer = QtCore.QTimer()
        self.progress_timer.timeout.connect(self.update_progress)

        self.snapshot_timer = QtCore.QTimer()

        self.setWindowTitle('Create a movie')
        ui.adjustSize()

        ui.lineEdit_filename.setFocus()

    def size_hint(self):
        size = QtCore.QSize(400, 100)
        return size

    def enable_size_widgets(self, checked):
        ui = self.ui
        for wid in [ui.label_template, ui.combobox_template,
                    ui.label_width, ui.lineedit_width,
                    ui.label_height, ui.lineedit_height]:
            wid.setEnabled(not checked)

    def update_progress(self):
        if self.ffmpeg_process is not None:
            poll = self.ffmpeg_process.poll()
            if poll is None:

                # collect a line, break at \r
                line = bytearray()
                while True:
                    c = self.ffmpeg_process.stderr.read(1)
                    line.extend(c)
                    if c in b"\r\n":
                        break

                if self.duration is None:
                    search = FFMPEG_DURATION_RX.search(line)
                    if search is not None:
                        self.duration = seconds(*search.groups())

                if self.duration is not None:
                    search = FFMPEG_TIME_RX.search(line)
                    if search is not None:
                        current = seconds(*search.groups())
                        self.ui.progressBar.setValue(int(current/self.duration*100))
            else:
                self.movie_finished()

    def movie_finished(self, progress=100):
        self.progress_timer.stop()
        self.snapshot_timer.stop()
        self.ui.progressBar.setValue(progress)
        self.ui.pushButton_create.setText('Create')
        self.ffmpeg_process = None

    def create_movie(self):
        # check process to see if we are canceling
        if self.ffmpeg_process is not None:
            self.ffmpeg_process.kill()
            self.movie_finished(progress=0)
            return

        ui = self.ui

        name = ui.lineEdit_filename.text() + ui.comboBox_ext.currentText()
        path = os.path.join(ui.lineEdit_path.text(), name)
        fps = ui.spinBox_fps.value()
        start = ui.spinBox_start.value()
        end = ui.spinBox_end.value()

        self.duration = None
        ui.progressBar.setValue(0)
        ui.progressBar.show()

        ui.pushButton_create.setText('Cancel')

        if self.ui.groupBox_use_image_stack.isChecked():
            self.progress_timer.start(10)
            self.process_image_stack(path, fps, start, end)
        else:
            self.process_pipe(path, fps, start, end)

    def process_image_stack(self, path, fps, start, end):
        ui = self.ui

        first_image = ui.lineEdit_image_stack_path.text()

        padcount = len(re.findall('(\d+)', first_image)[-1])
        wildcard = re.sub(r'\d+', f'd{padcount}0%', first_image[::-1], 1)[::-1]

        # ffmpeg command
        self.ffmpeg_process = (
            ffmpeg
            .input(wildcard, framerate=fps, start_number=start)
            .output(path, pix_fmt='yuv420p', vcodec='libx264', vframes=end-start)
            .run_async(overwrite_output=True, pipe_stdout=True, pipe_stderr=True)
        )

    def process_pipe(self, path, fps, start, end):

        ui = self.ui
        width = int(ui.lineedit_width.value)
        height = int(ui.lineedit_height.value)

        self.ffmpeg_process = (
            ffmpeg
            .input('pipe:', framerate=fps, format='rawvideo', pix_fmt='rgb24', s='{}x{}'.format(width, height))
            .vflip()
            .output(path, pix_fmt='yuv420p', vcodec='libx264')
            .run_async(pipe_stdin=True, overwrite_output=True)
        )

        self.parent.change_frame(start)
        self.take_snapshot()

    def take_snapshot(self):
        if self.ffmpeg_process is None:
            return

        ui = self.ui
        width = int(ui.lineedit_width.value)
        height = int(ui.lineedit_height.value)

        # get the new image
        im = self.parent.screenshot(False, fname=None, size=(width, height), raw=True)

        # convert vtk to numpy
        rows, cols, _ = im.GetDimensions()
        sc = im.GetPointData().GetScalars()
        array = vtk_to_numpy(sc)
        array = array.reshape(rows, cols, -1)

        # send the bytes to ffmpeg through a pipe
        self.ffmpeg_process.stdin.write(array.astype(np.uint8).tobytes())

        # change the frame
        start = ui.spinBox_start.value()
        end = ui.spinBox_end.value()
        current_frame = self.parent.frame_index + 1

        if current_frame > end:
            self.ffmpeg_process.stdin.close()
            self.movie_finished()
        else:
            ui.progressBar.setValue(int(((current_frame-start)/(end-start))*100))
            self.parent.change_frame(current_frame)
            self.snapshot_timer.singleShot(0, self.take_snapshot)

    def change_size(self, index=None):

        text = self.ui.combobox_template.currentText()

        w, h = None, None
        if '540p' in text:
            w, h = 960, 540
        elif '720p' in text:
            w, h = 1080, 720
        elif '1080p' in text:
            w, h = 1920, 1080
        elif '4K' in text:
            w, h = 3840, 2160

        if w is not None:
            self.ui.lineedit_width.updateValue('none', w)
            self.ui.lineedit_height.updateValue('none', h)

    def popup(self):
        ui = self.ui

        # get the current max frames
        nframes = max(len(self.parent.vtu_files), len(self.parent.vtp_files))-1
        ui.spinBox_start.setMaximum(nframes-1)
        ui.spinBox_end.setMaximum(nframes)
        ui.spinBox_end.setValue(nframes)

        # get the current size
        cur_size = self.parent.vtkRenderWindow.GetSize()
        cur_size = [int(s/2)*2 for s in cur_size]
        ui.lineedit_width.updateValue('none', cur_size[0])
        ui.lineedit_height.updateValue('none', cur_size[1])

        self.show()
        self.raise_()
        self.activateWindow()

    def browse(self):
        filename = QtWidgets.QFileDialog.getExistingDirectory(
            self,
            "Save screenshot",
            self.ui.lineedit_path.text())
        if isinstance(filename, (tuple, list)):
            filename = filename[0]
        if not filename:
            return
        self.ui.lineedit_path.setText(filename)

    def browse_image_stack(self):
        filename = QtWidgets.QFileDialog.getOpenFileName(
            self,
            "Select first image of stack",
            self.ui.lineEdit_image_stack_path.text(),
            "Images (*.png *.jpg)")
        if isinstance(filename, (tuple, list)):
            filename = filename[0]
        if not filename:
            return
        self.ui.lineEdit_image_stack_path.setText(filename)
