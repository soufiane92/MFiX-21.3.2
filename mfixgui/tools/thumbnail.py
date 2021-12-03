import argparse
import os

from qtpy.QtCore import Qt, QSize
from qtpy.QtGui import QImage, QPainter

from mfixgui.constants import *
from mfixgui.tools.qt import get_image_path


def create_project_thumbnail(mfixgui, save_gui_info=False, test=False):
    """create a thumbnail for the project"""

    path = os.path.join(mfixgui.get_project_dir(), ".thumbnail")

    cur_widget = mfixgui.ui.tabWidgetGraphics.currentWidget()
    # if vtk 7+ and the widget is not visible, the screenshot will be empty
    # don't bother saving the thumbnail
    if not cur_widget.isVisible() and not test:
        return

    # collect meta data
    solver = mfixgui.project.solver
    solver_dict = {
        SINGLE: "single",
        TFM: "tfm",
        DEM: "dem",
        CGP: "cgp",
        PIC: "pic",
        HYBRID: "hybrid",
    }
    single = solver_dict.get(solver, "single")

    geo = mfixgui.project.get_value("cartesian_grid", False)
    chem = bool(mfixgui.project.reactions)
    des = mfixgui.project.get_value("description")

    # try to get image from vtk
    temp = os.path.join(mfixgui.get_project_dir(), "temp.png")
    # find the currently visible vtk widget
    current_index = mfixgui.ui.tabWidgetGraphics.currentIndex()
    if hasattr(cur_widget, "vtk_widget"):
        cur_widget = cur_widget.vtk_widget
    elif not hasattr(cur_widget, "screenshot"):
        mfixgui.ui.tabWidgetGraphics.setCurrentWidget(mfixgui.ui.widgetModelGraphics)
        cur_widget = mfixgui.vtkwidget
    cur_widget.screenshot(True, temp, size=[400, 400], transparent=True, thumbnail=True)

    # create the thumbnail
    create_thumbnail(path, single, geo, chem, temp)
    if os.path.exists(temp):
        os.remove(temp)

    # save the model types too!
    if save_gui_info:
        path = os.path.join(mfixgui.get_project_dir(), ".mfixguiinfo")
        with open(path, "w", encoding="utf-8", errors="replace") as mfixguiinfo:
            mfixguiinfo.write(",".join(str(v) for v in [single, geo, chem, des]))

    # reset current tab
    mfixgui.ui.tabWidgetGraphics.setCurrentIndex(current_index)


def create_thumbnail(name, model, geometry, chemistry, background):
    if background is not None and os.path.exists(background):
        img = QImage(background)
        base = img.scaled(300, 300).scaled(128, 128, Qt.IgnoreAspectRatio)
    else:
        base = QImage(QSize(128, 128), QImage.Format_ARGB32)
        base.fill(Qt.white)

    painter = QPainter(base)

    # add images
    model = QImage(get_image_path(model + ".svg"))
    painter.drawImage(0, 128 - 24, model)

    if geometry:
        geo = QImage(get_image_path("geometry.svg"))
        painter.drawImage(24, 128 - 24, geo)

    if chemistry:
        geo = QImage(get_image_path("chemistry.svg"))
        painter.drawImage(24 * 2, 128 - 24, geo)

    base.save(name, "PNG")
    del painter


def main():

    model_types = ["single", "tfm", "dem", "pic", "hybrid"]

    parser = argparse.ArgumentParser(description="Create thumbnails for projects.")

    parser.add_argument(
        "project",
        action="store",
        default=None,
        help="project directory to save the thumbnail too",
    )
    parser.add_argument(
        "-m",
        "--model",
        metavar="MODEL",
        action="store",
        default="single",
        choices=model_types,
        help="specify model %s" % model_types,
    )
    parser.add_argument("-g", "--geo", action="store_true", help="cut-cell")
    parser.add_argument("-c", "--chem", action="store_true", help="chemistry")
    parser.add_argument(
        "-i",
        "--image",
        metavar="IMAGE",
        action="store",
        default=None,
        help="path to image",
    )
    args = parser.parse_args()
    out = os.path.join(args.project, ".thumbnail")
    create_thumbnail(out, args.model, args.geo, args.chem, args.image)


if __name__ == "__main__":
    main()
