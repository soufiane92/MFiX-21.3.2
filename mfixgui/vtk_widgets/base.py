"""
This is a base class for the vtk widgets.
"""

from qtpy import QtCore, QtWidgets, QtGui

import vtk
from vtk.qt.QVTKRenderWindowInteractor import QVTKRenderWindowInteractor

from mfixgui.tools.qt import get_icon, sub_icon_size, SETTINGS, find_gui, make_toolbutton
from mfixgui.vtk_widgets import VTK_MAJOR_VERSION
from mfixgui.vtk_widgets.screenshot_dialog import ScreenshotDialog



class BaseVtkWidget(QtWidgets.QWidget):
    def __init__(self, parent=None):
        QtWidgets.QWidget.__init__(self, parent)

        self.gui = find_gui(self)

        self.defer_render = False
        self.view_flip = [False]*3
        self.offscreen_vtkrenderer = None

        # scalar bar
        self.scalar_bar = None
        self.scalar_bar_position = 'right'
        self.scalar_bar_shadow = False
        self.scalar_bar_italic = False
        self.scalar_bar_label_fmt = '%.2f'
        self.scalar_bar_color = [0, 0, 0]
        self.scalar_bar_label = None
        self.scalar_bar_n_labels = 10

        self.screenshot_dialog = ScreenshotDialog(self)

        # --- layout ---
        self.grid_layout = QtWidgets.QGridLayout(self)
        self.grid_layout.setContentsMargins(0, 0, 0, 0)
        self.grid_layout.setSpacing(0)

        # https://vtk.org/Wiki/VTK_Mouse_Picking
        qrw = self.vtkWindowWidget = QVTKRenderWindowInteractor(self)
        qrw._mousePressEvent = qrw.mousePressEvent
        qrw._mouseMoveEvent = qrw.mouseMoveEvent
        qrw._mouseReleaseEvent = qrw.mouseReleaseEvent
        qrw.moving = False
        qrw.pressed = False
        qrw.mousePressEvent = self._mousePressEvent
        qrw.mouseMoveEvent = self._mouseMoveEvent
        qrw.mouseReleaseEvent = self._mouseReleaseEvent

        self.vtkWindowWidget.setSizePolicy(
            QtWidgets.QSizePolicy.Expanding,
            QtWidgets.QSizePolicy.Expanding)
        self.grid_layout.addWidget(self.vtkWindowWidget, 1, 0)

        # --- setup vtk stuff ---
        self.vtkrenderer = vtk.vtkRenderer()
        c1 = QtGui.QColor(SETTINGS.value('vtk_bck_color1', '#666666'))
        c2 = QtGui.QColor(SETTINGS.value('vtk_bck_color2', '#ffffff'))
        self.set_background_color(c1.getRgbF()[:3], c2.getRgbF()[:3])

        self.vtkRenderWindow = self.vtkWindowWidget.GetRenderWindow()
        self.vtkRenderWindow.AddRenderer(self.vtkrenderer)
        self.vtkRenderWindow.SetBorders(0)
        self.vtkRenderWindow.SetAlphaBitPlanes(1)
        self.vtkiren = self.vtkWindowWidget.GetRenderWindow().GetInteractor()

        #self.style = CustomInteractorStyle()
        self.style_3d = vtk.vtkInteractorStyleTrackballCamera()
        self.style_3d.SetDefaultRenderer(self.vtkrenderer)
        self.style_2d = vtk.vtkInteractorStyleImage()
        self.style_2d.SetDefaultRenderer(self.vtkrenderer)
        self.vtkiren.SetInteractorStyle(self.style_3d)

        # Orientation Arrows Marker Widget
        self.axes = vtk.vtkAxesActor()
        self.axes.AxisLabelsOn()
        self.axes.SetXAxisLabelText("X")
        self.axes.GetXAxisCaptionActor2D().GetCaptionTextProperty().SetColor(1, 0, 0)
        self.axes.GetXAxisCaptionActor2D().GetCaptionTextProperty().ShadowOff()
        self.axes.SetYAxisLabelText("Y")
        self.axes.GetYAxisCaptionActor2D().GetCaptionTextProperty().SetColor(0, 1, 0)
        self.axes.GetYAxisCaptionActor2D().GetCaptionTextProperty().ShadowOff()
        self.axes.SetZAxisLabelText("Z")
        self.axes.GetZAxisCaptionActor2D().GetCaptionTextProperty().SetColor(0, 0, 1)
        self.axes.GetZAxisCaptionActor2D().GetCaptionTextProperty().ShadowOff()

        # Orientation Marker Widget
        self.orientation_widget = vtk.vtkOrientationMarkerWidget()
        self.orientation_widget.SetOutlineColor(0.9300, 0.5700, 0.1300)
        self.orientation_widget.SetOrientationMarker(self.axes)
        self.orientation_widget.SetInteractor(self.vtkiren)
        self.orientation_widget.SetViewport(0.0, 0.0, 0.2, 0.2)
        self.orientation_widget.SetEnabled(1)
        self.orientation_widget.InteractiveOff()

        # --- time label ---
        self.time_label = vtk.vtkTextActor()
        self.time_label.SetVisibility(False)
        self.time_label.GetPositionCoordinate().SetCoordinateSystemToNormalizedViewport()
        self.time_label.GetPosition2Coordinate().SetCoordinateSystemToNormalizedViewport()
        self.time_label.SetPosition(.99, .95)
        tprop = self.time_label.GetTextProperty()
        tprop.SetFontSize(24)
        tprop.SetJustificationToRight()
        tprop.SetVerticalJustificationToTop()
        self.vtkrenderer.AddActor2D(self.time_label)
        self.set_timelabel(color=[0, 0, 0])

    # Overrides for mouse picking while still using mouse for camera control
    #   https://vtk.org/Wiki/VTK_Mouse_Picking
    def _mousePressEvent(self, ev):
        qrw = self.vtkWindowWidget
        qrw.moving = False
        qrw.pressed = True
        return qrw._mousePressEvent(ev)

    def _mouseMoveEvent(self, ev):
        qrw = self.vtkWindowWidget
        qrw.moving = True
        if qrw.pressed:
            return qrw._mouseMoveEvent(ev)

    def _mouseReleaseEvent(self, ev):
        qrw = self.vtkWindowWidget
        qrw.pressed = False
        if qrw.moving:
            qrw.moving = False
            return qrw._mouseReleaseEvent(ev)
        else:
            x,y = ev.x(), ev.y()
            sb = self.scalar_bar
            if not sb:
                return
            c = sb.GetPositionCoordinate()
            if not c:
                return
            if not self.sidebar:
                return
            x0, y0 = c.GetComputedDisplayValue(self.vtkrenderer)
            x1, y1 = sb.GetPosition2Coordinate().GetComputedDisplayValue(self.vtkrenderer)
            if x0 <= x <= x1 and y0 <= y <= y1:
                if self.sidebar.comboBox_color_bar_field.currentText().lower() == 'points':
                    self.handle_change_color('points', self.sidebar.toolButton_points_color)
                else:
                    self.handle_change_color('cells', self.sidebar.toolButton_cells_color)


    def init_base_toolbar(self):
        """add base toolbar"""
        self.button_bar = QtWidgets.QWidget(self)
        self.button_bar.setObjectName('button_bar')
        self.button_bar_layout = QtWidgets.QHBoxLayout(self.button_bar)
        self.button_bar_layout.setContentsMargins(0, 0, 0, 0)
        self.button_bar_layout.setSpacing(0)
        self.button_bar.setLayout(self.button_bar_layout)
        # self.button_bar.setGeometry(QtCore.QRect(0, 0, 300, 300))
        self.grid_layout.addWidget(self.button_bar, 0, 0, 1, -1)

        size = sub_icon_size()
        self.toolbutton_reset = make_toolbutton(
            'overscan.svg', self.reset_view, 'Reset View', size)
        self.toolbutton_perspective = make_toolbutton(
            'perspective.svg', lambda ignore: self.perspective(), 'Perspective', size)
        self.toolbutton_view_xy = make_toolbutton(
            'xy.svg', lambda: self.set_view('xy'), 'XY View', size)
        self.toolbutton_view_yz = make_toolbutton(
            'yz.svg', lambda: self.set_view('yz'), 'YZ View', size)
        self.toolbutton_view_xz = make_toolbutton(
            'xz.svg', lambda: self.set_view('xz'), 'XZ View', size)
        self.toolbutton_rotate_left = make_toolbutton(
            'rotate_left.svg', lambda: self.rotate(90), 'Rotate counter-clockwise', size)
        self.toolbutton_rotate_right= make_toolbutton(
            'rotate_right.svg', lambda: self.rotate(-90), 'Rotate clockwise', size)
        self.toolbutton_screenshot = make_toolbutton(
            'camera.svg', self.screenshot, 'Save scene as image', size)

        for btn in [self.toolbutton_reset,
                    self.toolbutton_view_xy,
                    self.toolbutton_view_yz,
                    self.toolbutton_view_xz,
                    self.toolbutton_rotate_left,
                    self.toolbutton_rotate_right,
                    self.toolbutton_perspective,
                    self.toolbutton_screenshot]:
            self.button_bar_layout.addWidget(btn)
            btn.setAutoRaise(True)
            btn.setFocusPolicy(QtCore.Qt.ClickFocus)

    # --- render ---
    def render(self, force_render=False, defer_render=None):
        """render the vtk scene, checks for defer_render"""
        if defer_render is not None:
            self.defer_render = defer_render
        if not self.defer_render or force_render:
            self.vtkRenderWindow.Render()
            # find out who is calling render
            # curframe = inspect.currentframe()
            # calframe = inspect.getouterframes(curframe, 2)
            # print('Render', type(self).__name__, 'caller:', calframe[1][3])

    def screenshot(self, checked, fname=None, size=(1920, 1080),
                   thumbnail=False, transparent=False, raw=False):
        """take a snapshot of the vtk window"""
        self.toolbutton_screenshot.setDown(False)

        if not raw and fname is None:
            success, fname, size, transparent = self.screenshot_dialog.get()
            if not success:
                return

        cur_size = self.vtkRenderWindow.GetSize()

        if int(SETTINGS.value('enable_screenshot_res', 0)):
            self.vtkRenderWindow.SetSize(*size)
        elif thumbnail:
            self.vtkRenderWindow.SetSize(*size)
        else:
            # force the current res to be even (for making videos)
            size = [int(s/2)*2 for s in cur_size]
            self.vtkRenderWindow.SetSize(*size)

        if transparent:
            self.vtkrenderer.SetLayer(1)
        else:
            self.vtkrenderer.SetLayer(0)

        # screenshot code:
        window_image = vtk.vtkWindowToImageFilter()
        window_image.SetInput(self.vtkRenderWindow)
        if transparent:
            window_image.SetInputBufferTypeToRGBA()
        window_image.ReadFrontBufferOff()
        if hasattr(window_image, 'SetScale'):
            window_image.SetScale(1)
        else:
            window_image.SetMagnification(1)

        if not raw:
            if fname.endswith('.png'):
                writer = vtk.vtkPNGWriter()
            elif fname.endswith('.jpg'):
                writer = vtk.vtkJPEGWriter()
            elif fname.endswith('.ps'):
                writer = vtk.vtkPostScriptWriter()
            else:
                # force to png
                writer = vtk.vtkPNGWriter()
                fname = fname.endswith('.png')

            writer.SetFileName(str(fname))
            writer.SetInputConnection(window_image.GetOutputPort())

        self.vtkRenderWindow.Render()
        window_image.Update()

        if not raw:
            writer.Write()

        # rest size
        self.vtkRenderWindow.SetSize(cur_size)
        self.vtkrenderer.SetLayer(0)
        self.render()

        if raw:
            return window_image.GetOutput()

    def change_interaction(self, style_2d=False):
        if style_2d:
            self.vtkiren.SetInteractorStyle(self.style_2d)
            self.view_flip[1] = False
            self.set_view()
            enabled = False
        else:
            self.vtkiren.SetInteractorStyle(self.style_3d)
            self.perspective(False)
            enabled = True

        for btn in [self.toolbutton_perspective, self.toolbutton_view_yz,
                    self.toolbutton_view_xz]:
            btn.setEnabled(enabled)

    def get_camera_state(self):
        camera = self.vtkrenderer.GetActiveCamera()
        pos = camera.GetPosition()
        focal = camera.GetFocalPoint()
        view_angle = camera.GetViewAngle()
        view_up = camera.GetViewUp()
        parallel = camera.GetParallelProjection()
        parallel_scale = camera.GetParallelScale()

        return {
            'position': pos,
            'focal_point': focal,
            'view_angle': view_angle,
            'view_up': view_up,
            'parallel': parallel,
            'parallel_scale': parallel_scale
            }

    def set_camera_state(self, state):
        camera = self.vtkrenderer.GetActiveCamera()

        par = state.get('parallel', None)
        if par is not None:
            self.perspective(par)

        pos = state.get('position', None)
        if pos is not None:
            camera.SetPosition(pos)

        foc = state.get('focal_point', None)
        if foc is not None:
            camera.SetFocalPoint(foc)

        vangle = state.get('view_angle', None)
        if vangle is not None:
            camera.SetViewAngle(vangle)

        vup = state.get('view_up', None)
        if vup is not None:
            camera.SetViewUp(vup)

        ps = state.get('parallel_scale', None)
        if ps is not None:
            camera.SetParallelScale(ps)

    def perspective(self, parallel=None):
        """change the perspective of the vtk scene"""
        camera = self.vtkrenderer.GetActiveCamera()

        if parallel is None:
            parallel = not camera.GetParallelProjection()

        if parallel:
            camera.ParallelProjectionOn()
            self.toolbutton_perspective.setIcon(get_icon('parallel.svg'))
        else:
            camera.ParallelProjectionOff()
            self.toolbutton_perspective.setIcon(get_icon('perspective.svg'))

        self.render()

    def set_view(self, view='xy'):
        """set the 2D view of the scene"""
        self.perspective(parallel=True)
        camera = self.vtkrenderer.GetActiveCamera()
        if view == 'xy':
            camera.SetPosition(0, 0, 10000000)
            camera.SetViewUp(0, 1, 0)
            if self.view_flip[1]:
                camera.Azimuth(180)
            self.view_flip[1] = not self.view_flip[1]
        elif view == 'xz':
            camera.SetPosition(0, 10000000, 0)
            camera.SetViewUp(1, 0, 0)
            if self.view_flip[2]:
                camera.Azimuth(180)
            self.view_flip[2] = not self.view_flip[2]
        elif view == 'yz':
            camera.SetPosition(10000000, 0, 0)
            camera.SetViewUp(0, 1, 0)
            if self.view_flip[2]:
                camera.Azimuth(180)
            self.view_flip[2] = not self.view_flip[2]

        self.reset_view()

    def rotate(self, degrees):
        camera = self.vtkrenderer.GetActiveCamera()
        camera.Roll(degrees)
        self.render()

    def reset_view(self):
        """reset the camera so that the geometry is visible in the scene"""
        self.vtkrenderer.ResetCamera()
        self.render()

    def set_background_color(self, color1=None, color2=None):
        self.vtkrenderer.GradientBackgroundOn()
        self.vtkrenderer.SetBackground(color1)
        self.vtkrenderer.SetBackground2(color2)

    def set_colorbar(self, mapper=None, label=None, position=None, color=None,
                     shadow=None, italic=None, n_labels=None, label_fmt=None):
        # save options so they persist, even if the color_bar is not instantiated
        # the color bar only works if there is a mapper associated with it, else
        # vtk produces numerous errors
        if label is not None:
            self.scalar_bar_label = label
        if n_labels is not None:
            self.scalar_bar_n_labels = n_labels
        if position is not None:
            self.scalar_bar_position = position
        if color is not None:
            self.scalar_bar_color = color
        if shadow is not None:
            self.scalar_bar_shadow = shadow
        if italic is not None:
            self.scalar_bar_italic = italic
        if label_fmt is not None:
            self.scalar_bar_label_fmt = label_fmt

        if self.scalar_bar is None and mapper is not None:
            self.scalar_bar = vtk.vtkScalarBarActor()
            self.scalar_bar.GetPositionCoordinate().SetCoordinateSystemToNormalizedViewport()
            self.scalar_bar.AnnotationTextScalingOff()
            self.vtkrenderer.AddActor(self.scalar_bar)

        # return early if there is not scalar_bar
        if self.scalar_bar is None:
            return

        if self.scalar_bar_position is not None:
            geo = [0.08, 0.7]
            position = self.scalar_bar_position

            if position == 'left':
                pos = [0.02, 0.15]
                self.scalar_bar.SetOrientationToVertical()
            elif position == 'bottom':
                pos = [0.15, 0.02]
                geo = list(reversed(geo))
                self.scalar_bar.SetOrientationToHorizontal()
            elif position == 'top':
                pos = [0.15, 0.9]
                geo = list(reversed(geo))
                self.scalar_bar.SetOrientationToHorizontal()
            else: # default to right
                pos = [0.9, 0.15]
                self.scalar_bar.SetOrientationToVertical()

            self.scalar_bar.GetPositionCoordinate().SetValue(*pos)
            self.scalar_bar.SetWidth(geo[0])
            self.scalar_bar.SetHeight(geo[1])


        if mapper is not None:
            self.scalar_bar.SetLookupTable(mapper.GetLookupTable())

        if self.scalar_bar_label is not None:
            self.scalar_bar.SetTitle(self.scalar_bar_label)

        if self.scalar_bar_n_labels is not None:
            self.scalar_bar.SetNumberOfLabels(self.scalar_bar_n_labels)

        if self.scalar_bar_color is not None:
            for label in [self.scalar_bar.GetLabelTextProperty(), self.scalar_bar.GetTitleTextProperty()]:
                label.SetColor(self.scalar_bar_color)

        if self.scalar_bar_shadow is not None:
            for label in [self.scalar_bar.GetLabelTextProperty(), self.scalar_bar.GetTitleTextProperty()]:
                label.SetShadow(self.scalar_bar_shadow)

        if self.scalar_bar_italic is not None:
            for label in [self.scalar_bar.GetLabelTextProperty(), self.scalar_bar.GetTitleTextProperty()]:
                label.SetItalic(self.scalar_bar_italic)

        if self.scalar_bar_label_fmt is not None:
            self.scalar_bar.SetLabelFormat(self.scalar_bar_label_fmt)

    def set_timelabel(self, text=None, fontsize=None, pos=None, color=None):
        text_prop = self.time_label.GetTextProperty()
        if text is not None:
            self.time_label.SetInput(text)

        if fontsize is not None:
            text_prop.SetFontSize(fontsize)

        if color is not None:
            text_prop.SetColor(color)

        if pos is not None:
            if 'left' in pos:
                text_prop.SetJustificationToLeft()
            elif 'center' in pos:
                text_prop.SetJustificationToCentered()
            else:
                text_prop.SetJustificationToRight()
            if 'top' in pos:
                text_prop.SetVerticalJustificationToTop()
            else:
                text_prop.SetVerticalJustificationToBottom()

            if pos == 'top left':
                pos = [0.01, 0.99]
            elif pos == 'top center':
                pos = [0.5, 0.99]
            elif pos == 'top right':
                pos = [0.99, 0.99]
            elif pos == 'bottom right':
                pos = [0.99, 0.0]
            elif pos == 'bottom center':
                pos = [0.5, 0.0]
            elif pos == 'bottom left':
                pos = [0.01, 0.0]

            self.time_label.SetPosition(*pos)

    def set_representation(self, actor, rep):
        """set the representation of an actor"""
        if actor is None: return
        if rep == 'wire':
            actor.GetProperty().SetRepresentationToWireframe()
        elif rep == 'solid':
            actor.GetProperty().SetRepresentationToSurface()
            actor.GetProperty().EdgeVisibilityOff()
        elif rep == 'edges':
            actor.GetProperty().SetRepresentationToSurface()
            actor.GetProperty().EdgeVisibilityOn()
        elif rep == 'points':
            actor.GetProperty().SetRepresentationToPoints()
