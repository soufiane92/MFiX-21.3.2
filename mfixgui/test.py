#from mfixgui.gui import gui
from mfixgui.advanced import advanced_keys
from mfixgui.locate_keyword import locate_keyword

import mfixgui.namespace
gui = mfixgui.namespace.gui

gui_keys = sorted(list(set(gui.keyword_doc.keys()) - advanced_keys))

def run():
    n_ok = n_not_found = n_fail = 0

    gui.change_mode('modeler')

    for key in gui_keys:
        # Handle any pending notifications before changing tabs
        # This avoids errors when changing from solid to fluid tab,
        #  where P=None in callback.  Does not happen in normal
        #  use since the user would have triggered the focus-out event
        w = gui.focusWidget()
        if w:
            w.clearFocus()
        try:
            found, msg = locate_keyword(gui, key)
            if found:
                msg = "OK"
                n_ok += 1
            else:
                msg = "NOT FOUND: %s" % msg
                n_not_found += 1
        except Exception as e:
            msg = "FAIL %s" % e
            n_fail += 1
            raise

        gui.print_internal("%s %s" % (key, msg))
        gui.app.processEvents()

    gui.print_internal("tried %d keys" % len(gui_keys))
    gui.print_internal("%d found" % n_ok)
    gui.print_internal("%d not found" % n_not_found)
    gui.print_internal("%d errors" % n_fail)
