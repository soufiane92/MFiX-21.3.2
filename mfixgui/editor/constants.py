from qtpy.QtCore import Qt
from qtpy.QtGui import QColor

# similar to Atom's "one light" theme
DEFAULT_COLOR_SCHEME = {
      "background":     Qt.white,
      "currentline":    QColor('#e9e9e9'),
      "currentcell":    Qt.lightGray,
      "sideareas":      QColor('#e9e9e9'),
      "sidetext":       QColor('#a0a1a7'),
      "title":          QColor('#005e99'),
      "matched_p":      QColor('#383a42'),
      "unmatched_p":    QColor('#839496'),
      "normal":         QColor('#383a42'),
      "keyword":        QColor('#a626a4'),
      "builtin":        QColor('#986801'),
      "definition":     QColor('#0184bc'),
      "comment":        QColor('#a0a1a7'),
      "string":         QColor('#50a14f'),
      "number":         QColor('#9a6a05'),
      "instance":       QColor('#e45649'),
      }

LANGUAGE_DEFAULTS = {
    'fortran77': {
        'exts':     ['f', 'for', 'f77'],
        'tab_char': '   ',
        },
    'fortran': {
        'exts':     ['f90', 'f95', 'f2k', 'f03', 'f08', 'inc'],
        'tab_char': '   ',
        },
    'python': {
        'exts':     ['py', 'pyw', 'python', 'ipy'],
        'tab_char': '    ',
        },
    'cpp': {
        'exts':     ['c', 'cc', 'cpp', 'cxx', 'h', 'hh', 'hpp', 'hxx'],
        'tab_char': '  ',
        },
    'text': {
        'exts':     ['txt', 'log', 'out', 'csv'],
        'tab_char': '\t',
        },
    'yaml': {
        'exts':     ['yaml', 'yml'],
        'tab_char': '  ',
        },
    'markdown': {
        'exts':     ['md', 'mdw'],
        'tab_char': '  ',
        },
    'mfix': {
        'exts':     ['mfx', 'dat'],
        'tab_char': '   ',
        },
    }

OPERATORS = {'+', '-', '*', '**', '/', '//', '%', '@', '<<', '>>',
             '&', '|', '^', '~', '<', '>', '<=', '>=', '==', '!='}

DELIMITERS = {' ', '.', ',', ':', ';', '@', '=', '->', '+=', '-=', '*=', '/=',
              '//=', '%=', '@=', '&=', '|=', '^=', '>>=', '<<=', '**=',
              '{', '}', '[', ']', '(', ')'}
