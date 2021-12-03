from qtpy.QtCore import (QAbstractAnimation, QEasingCurve, QLine,
                         QParallelAnimationGroup, QPoint, QPropertyAnimation,
                         QRect, QSize)

from qtpy.QtWidgets import QGridLayout, QPushButton, QWidget, QWidgetItem

from mfixgui.tools.qt import SETTINGS

def animation_speed():
    return int(SETTINGS.value('animation_speed', 200))
# --- animation methods ---
def animate_stacked_widget(mfixgui,
                           stackedwidget,
                           start_end,
                           direction = 'horizontal',
                           line = None,
                           to_btn = None,
                           btn_layout = None):
    # check to see if already animating
    if mfixgui.animating and mfixgui.stack_animation is not None:
        mfixgui.stack_animation.stop()

    start, end = start_end

    from_widget = stackedwidget.widget(start)
    to_widget = stackedwidget.widget(end)

    dimensions = (from_widget.frameGeometry().width(),
                  from_widget.frameGeometry().height())

    mfixgui.stack_animation = QParallelAnimationGroup()

    if start != end:
        animate_from_to(mfixgui,
                        (to_widget, from_widget),
                        direction,
                        start_end,
                        dimensions)

    # to_btn may be either a Widget or a WidgetItem.  In the latter case,
    # extract the widget
    if to_btn:
        if isinstance(to_btn, QWidgetItem):
            to_btn = to_btn.widget()

    line_to = None # not all animations have a line
    if line and to_btn:
        line_to = animate_line(mfixgui, line, to_btn, btn_layout)
    mfixgui.stack_animation.stateChanged.connect(lambda old, new, mfixgui=mfixgui, stackedwidget=stackedwidget, start_end=start_end, btn_layout=btn_layout, line=line, line_to=line_to:
                                                 animation_finished(mfixgui, stackedwidget, start_end,
                                                                    btn_layout, line, line_to))

    mfixgui.animating = True
    mfixgui.stack_animation.start()


def animate_from_to(mfixgui,
                     tofrom_widgets,
                     direction,
                     start_end,
                     dimensions):

    width, height = dimensions
    offsetx, offsety = get_offsets(direction, start_end, width, height)

    to_widget, from_widget = tofrom_widgets
    to_widget.setGeometry(0 + offsetx, 0 + offsety, width, height)
    to_widget.show()
    to_widget.raise_()

    animation_setup(mfixgui,
                    from_widget,
                    QPoint(0, 0),
                    QPoint(-offsetx, -offsety))

    animation_setup(mfixgui,
                    to_widget,
                    QPoint(offsetx, offsety),
                    QPoint(0, 0))

def get_offsets(direction,
                 start_end,
                 width,
                 height):
    start, end = start_end
    if direction == 'vertical':
        return  (0, (height if start < end else
                     -height))
    elif direction == 'horizontal':
        return ((width if start < end else
                 -width), 0)
    else:
        raise ValueError(f"Invalid direction: {direction}")


def animate_line(mfixgui,
                  line,
                  to_btn,
                  btn_layout=None):

    animation_setup(mfixgui,
                    line,
                    QPoint(line.geometry().x(), line.geometry().y()),
                    QPoint(to_btn.geometry().x(), line.geometry().y()),
                    to_btn.geometry().width())
    if btn_layout is not None:
        for i in range(0, btn_layout.columnCount()):
            item = btn_layout.itemAtPosition(0,i)
            if item:
                widget = item.widget()
                if widget == to_btn:
                    return i


def animation_setup(mfixgui,
                    target,
                    start_point,
                    end_point,
                    width_end=None):

    animation = QPropertyAnimation(target, b"pos")
    animation.setDuration(animation_speed())
    animation.setEasingCurve(QEasingCurve.InOutQuint)
    animation.setStartValue(start_point)
    animation.setEndValue(end_point)
    mfixgui.stack_animation.addAnimation(animation)

    # resize line width
    if width_end is not None:
        animation = QPropertyAnimation(target, b"size")
        animation.setDuration(animation_speed())
        animation.setEasingCurve(QEasingCurve.InOutQuint)
        size = target.size()
        animation.setStartValue(size)
        animation.setEndValue(QSize(width_end, size.height()))
        mfixgui.stack_animation.addAnimation(animation)


def animation_finished(mfixgui,
                       widget,
                       start_end,
                       btn_layout = None,
                       line = None,
                       line_to = None):
    mfixgui.animating = False
    start, end = start_end
    try:
        if mfixgui.stack_animation.state() == QAbstractAnimation.Stopped:
            widget.setCurrentIndex(end)
            if start != end:
                from_widget = widget.widget(start)
                from_widget.hide()
                from_widget.move(0, 0)
            if btn_layout is not None and line is not None:
                btn_layout.addItem(btn_layout.takeAt(
                    btn_layout.indexOf(line)), 1, line_to or end)
    except AttributeError:  #happens at shutdown
        raise

def make_widget_property_animation(mfixgui, start, stop):
    animation = QPropertyAnimation(mfixgui, b"geometry")
    animation.setDuration(animation_speed()),
    animation.setEasingCurve(QEasingCurve.InOutQuint)
    animation.setStartValue(start)
    animation.setEndValue(stop)
    return animation
