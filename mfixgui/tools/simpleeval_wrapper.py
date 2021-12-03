# MFIX-specific wrapper for library function 'simpleeval'

import math
import re
from mfixgui.tools import simpleeval

DEFAULT_FUNCTIONS = simpleeval.DEFAULT_FUNCTIONS

DEFAULT_FUNCTIONS.update({
    #    'rand': random,
    #    'randint': random_int,
    #    'int': int,
    #    'float': float,
    'abs': abs,
    'acos': math.acos,
    'asin': math.asin,
    'atan': math.atan,
    'bool': bool,
    'cos': math.cos,
    'degrees': math.degrees,
    'exp': math.exp,
    'limit': lambda a,b,c: max(a,min(b,c)),
    'log': math.log,
    'log10': math.log10,
    'max': max,
    'min': min,
    'pow': math.pow,
    'radians': math.radians,
    'sin': math.sin,
    'sqrt': math.sqrt,
    'tan': math.tan})

DEFAULT_NAMES = simpleeval.DEFAULT_NAMES
DEFAULT_NAMES.update({'pi': math.pi,
                      'e': math.e})


VALID_EXPRESION_NAMES = list(DEFAULT_FUNCTIONS.keys()) + list(DEFAULT_NAMES.keys())


d_pat = re.compile(r'([0-9]+\.?)([dD])([-+]?[0-9]+)')

def simple_eval(expr, names={}):
    ''' evaluate an expression '''

    # handle Fortran-style floats with 'd' - cgw 2016-11-14
    expr = d_pat.sub(r'\1e\3', expr)
    v = simpleeval.simple_eval(expr, names=names)

    # Round results so 3 * (1/3) == 1, etc
    if isinstance(v, float):
        v = round(v, 15)

    return v


def main():
    for expr in ('cos(2)',
                 'e + pi',
                 '3e4',
                 'degrees(pi)',
                 '3e+4',
                 '3d+4',
                 '3d4',
                 '3.d+4'
                 '1/2',
                 '3 * (1/3)',
                 '1/(1+sin(pi))',
                ):
        print("%s = %s" %(expr, simple_eval(expr)))

if __name__ == '__main__':
    main()
