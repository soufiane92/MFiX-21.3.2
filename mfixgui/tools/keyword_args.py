#!/usr/bin/env python

# Information about argument types for keywords,
# extracted from keyword documentation in mfix sources

from mfixgui.namelistparser import getKeywordDoc

doc = getKeywordDoc()

def normalize(arg):
    arg = arg.lower()
    if 'species' in arg:
        if arg != 'des_species':
            return 'species'
    return arg.split()[0].replace('+1','')

keyword_args = dict((k, tuple(normalize(a['id'])
                              for a in doc[k]['args'].values()))
                    for k in doc.keys())

arg_types = set()
for (k, v) in keyword_args.items():
    arg_types.update(v)

keys_by_type = {}
for t in arg_types:
    keys_by_type[t] = [k for (k, v) in keyword_args.items() if t in v]
    keys_by_type[t].sort()


def mkargs(key, **value_dict):
    args = keyword_args.get(key)
    if args is None:
        return
    if 'is_' in value_dict:
        value_dict['is'] = value_dict['is_']
    r = [value_dict.get(a, a) for a in args]
    return r


saved_keyword_args = keyword_args.copy()

def reset_keyword_args():
    keyword_args.clear()
    keyword_args.update(saved_keyword_args)

def add_keyword_args(keyword, arglist):
    if arglist is None:
        arglist = ()
    arglist = tuple(arglist)
    keyword_args[keyword] = arglist
    arg_types.update(arglist)
    for arg in arglist:
        if arg not in keys_by_type:
            keys_by_type[arg] = [keyword]
        else:
            keys_by_type[arg].append(keyword)
