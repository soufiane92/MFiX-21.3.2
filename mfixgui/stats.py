#!/usr/bin/env python
# -*- coding: utf-8 -*-
import sys
from math import sqrt


def get_val(tok):
    v=None
    try:
        v=float(tok)
    except ValueError: # Time data
        if ':' in tok:
            t=tok.split(':')
            if len(t) == 3:
                try:
                    h,m,s=map(float,t)
                    return s+60*(m+60*h)
                except ValueError:
                    pass
            elif len(t) == 2:
                #  Hmm, is this H:M or M:S?
                try:
                    m,s=map(float,t)
                    v = s+60*m
                except ValueError:
                    pass
        elif 'm' in tok and tok.endswith('s'):
        ### 4m3.718s format returned by "time"
            try:
                m,s=tok.split('m')
                s=s[:-1]
                v=float(s)+60*float(m)
            except ValueError:
                pass
    if verbose:
        print(tok, v)
    return v



verbose=False



data = []

def read_file(f):
    while True:
        line = f.readline()
        if verbose:
            print("Read", line)
        if not line:
            break
        line = line.strip()
        for tok in line.split():
            if not tok:
                continue
            v=get_val(tok)
            if v is not None:
                data.append(v)

implicit_stdin=True
for arg in sys.argv[1:]:
    if arg=='-v':
        verbose=True
        continue
    elif arg=='-':
        implicit_stdin=False
        read_file(sys.stdin)
    else:
        implicit_stdin=False
        f=open(arg,'r')
        read_file(f)
        f.close()

if implicit_stdin:
    read_file(sys.stdin)


N=len(data)
T=sum(data)
M=T/N
S=sqrt(sum([(x-M)**2 for x in data])/N)
print ("N=%d min=%.2f avg=%.2f max=%.2f σ=%.2f" %
       (N, min(data), M, max(data), S))
