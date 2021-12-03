# graphics libraries
try:
    import pyqtgraph as pg
    from pyqtgraph import PlotWidget
    PYQTGRAPH_AVAILABLE = True
    pg.setConfigOption('background', 'w')
    pg.setConfigOption('foreground', 'k')

    # monkey patch default method to plot widget
    def plotwidget_default(cls):
        '''default method to mix in to PlotWidget'''
        cls.clear()

    PlotWidget.default = plotwidget_default

except (ImportError, RuntimeError):
    pg = None
    PYQTGRAPH_AVAILABLE = False
    PlotWidget = None

TABLEAU20 = [(31, 119, 180),
             (255, 127, 14),
             (44, 160, 44),
             (214, 39, 40),
             (148, 103, 189),
             (140, 86, 75),
             (227, 119, 194),
             (127, 127, 127),
             (188, 189, 34),
             (219, 219, 141),
             (23, 190, 207),
             (158, 218, 229),
             (174, 199, 232),
             (255, 187, 120),
             (152, 223, 138),
             (255, 152, 150),
             (197, 176, 213),
             (196, 156, 148),
             (247, 182, 210),
             (199, 199, 199)
            ]


DEFAULT_PENS = [pg.mkPen(color=color, width=2)
                for color in TABLEAU20] if PYQTGRAPH_AVAILABLE else []
DEFAULT_BRUSHS = [pg.mkBrush(color=color)
                 for color in TABLEAU20] if PYQTGRAPH_AVAILABLE else []
