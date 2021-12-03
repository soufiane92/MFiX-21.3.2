#!/usr/bin/env python

# Make sure mfixgui imports succeed when running standalone
import sys, os
try:
    DIR=os.path.dirname(os.path.realpath(__file__))
    sys.path.insert(0, os.path.dirname(DIR))
except:
    DIR = '.'

import re, json
import time     # {time}
import getpass  # {user}
import platform # {host}

from itertools import product

from mfixgui.version import __version__
from mfixgui.project import Keyword
from mfixgui.tools.keyword_args import keyword_args, arg_types
from mfixgui.tools import format_key_with_args, break_string
from mfixgui.namelistparser import getKeywordDoc

import warnings

template_file = 'standard'

def load_template(name):
    global template_file
    global template_lines
    if os.path.isabs(name):
        template_file = name
    # Allow per-project override of 'output_templates'
    elif os.path.exists(os.path.join('output_templates', name)):
        template_file = os.path.join('output_templates', name)
    else:
        template_file = os.path.join(DIR, 'output_templates', name)

    if not os.path.exists(template_file): # Should this be the caller's responsibility?
        warn("Template '%s' not found, using standard template. Check MFiX settings."
             %name)
        name = 'standard'
        template_file = os.path.join(DIR, 'output_templates', name)
        if not os.path.exists(template_file):
            raise FileNotFoundError(template_file)

    template_lines = [l.rstrip() for l in open(template_file)]  # Remove trailing whitespace and newline

loop_ranges = {}
loop_values = {}
loop_vars = {}
all_keys = set()
if_val = True
empty_loop = False
lineno = 0
stack = []
proj = None
keyword_doc = getKeywordDoc() # For categories & defaults
categories = set(v.get('category') for v in keyword_doc.values())
defaults = dict((k, v.get('initpython')) for (k,v) in keyword_doc.items())


def err(msg):
    if lineno is not None:
        line = template_lines[lineno-1]
        raise ValueError("%s at %s:%s: %s"%(msg, template_file, lineno, line))
    else:
        raise ValueError(msg)

def warn(msg):
    warnings.warn(msg)

def get_loop_range(var):
    return sorted(loop_ranges.get(var, []))

def normalize(var):
    var = var.lower().strip()
    if var == 'p':
        return 'phase'
    elif var == 's':
        return 'species'
    elif var in ('mon', 'm'):
        return 'monitor'
    elif var in ('sc', 'scl'):
        return 'scalar'
    elif var == 'v':
        return 'vtk'
    elif var == 'eq':
        return 'equation'
    elif var == 'q':
        return 'quadric'
    elif var in ('resid', 'res'):
        return 'residual'
    elif var in ('udf', 'u', 'user'):
        return 'user-defined'
    elif var == 'cat':
        return 'category'
    return var

def start_loop(var, lineno):
    global empty_loop
    var0 = var
    if '(' in var: # Loop variable is dependent, used for species(phase)
        var, idx = var.split('(')
        var = var.strip()
        var = normalize(var)
        idx = idx.strip()
        if not idx.endswith(')'):
            err('Unmatched "({')
        idx = idx[:-1].strip()
        if idx.isdigit():
            idx = int(idx)
        else:
            idx = normalize(idx)
            if idx in loop_vars:
                idx = loop_vars[idx]
            else:
                err('Invalid loop variable "%s"' % var0)
    else:
        var = normalize(var)
        idx = 0 if var == 'species' else None
    if var not in arg_types and var != 'category':
        err('Unknown loop variable "%s"' %var0)
    for item in stack:
        if item[0] == 'loop' and item[1] == var:
            err('Loop variable "%s" is already active' % var)
    stack.append(('loop', var, lineno))
    values = loop_values[var] = get_loop_range(var if idx is None
                                               else (var, idx))
    if values:
        loop_vars[var] = values.pop(0)
        empty_loop = False
    else:
        loop_vars[var] = None
        empty_loop = True


def end_loop(end_var=None):
    global empty_loop
    empty_loop = False
    if not stack:
        err('Unmatched "{end loop"')
    loop = stack[-1]
    if loop[0] != 'loop':
        err('Unmatched "{end loop"')
    var, start_line = loop[1:]
    if end_var and end_var != var:
        err('Unmatched "{end loop", expected "%s", got "%s"' % (var, end_var))
    v = loop_values.get(var)
    if v:
        next_val = v.pop(0)
        loop_vars[var] = next_val
        return start_line
    else: # Got to end
        stack.pop()
        return lineno


def start_if(cond, lineno):
    global if_val
    stack.append(('if', if_val, lineno))
    # evaluate nested if, even if outer if is false, to catch errors
    if_val = eval_if(cond) and if_val


def end_if(cond):
    global if_val
    if not stack:
        err('No matching "{if"')
    if stack[-1][0] != 'if':
        err('Unmatched "{%s"'%stack[-1][0])
    if_val = stack.pop()[1]

last_match = (None, None) # value and (key, args) for last evaluated "{if" expression, needed for {_}
def eval_if(cond):
    global last_match
    use_default = '*' not in cond
    bval, repl, to_remove = _eval(cond, use_default)
    last_match = (repl, to_remove)
    return bval

builtins = {'date': time.ctime,
                 'version': __version__,
                 'user': getpass.getuser,
                 'hostname': platform.node,
                 'host': platform.node,
                 'comment_block': lambda: [l.rstrip() for l in getattr(proj, 'comment_block', [])]}


def args_typematch(expected, got):
    if len(expected) != len(got):
        return False
    for (e, g) in zip(expected, got):
        if g.isdigit():
            g = int(g)
        if isinstance(g, int):
            pass # Explicit int matches any type
        elif g=='*':
            pass # Wildcard matches anything
        elif e != g:
            return False
    return True

def args_match(expected, got):
    if len(expected) != len(got):
        return False
    for (e, g) in zip(expected, got):
        if e=='*' or e==g:
            pass
        else:
            return False
    return True

def get_category(arg):
    if arg == 'des':
        arg = 'discrete element simulation'
    elif arg == 'dem':
        arg = 'discrete element model'
    elif arg == 'tfm':
        arg = 'two fluid model'
    elif arg == 'pic':
        arg = 'particle in cell'
    elif arg in ('fluid', 'fluid phase'):
        arg = 'gas phase'
    elif arg == 'unknown':
        pass
    elif arg in('cat', 'category'):
        arg = loop_vars.get('category')
        if arg is None:
            err('loop var "%s" is not active' % arg)

    #elif not any(arg in cat for cat in categories):
    #    err('Unmatched category "%s"' % (arg))
    return arg

def _eval(expr, use_default=False):
    # This is shared by 'expand' and 'eval_if'
    # Evaluate the expression, returning a tuple containing
    #  Boolean value of expression
    #  String expansion, or list of strings
    #  List of (key, args) tuples that are matched from all_keys

    # Whitespace and case don't matter here
    expr = expr.strip().lower()

    # Split expression into key + args
    if '(' in expr:
        key, args = expr.split('(', 1)
        key = key.strip()
        args = args.strip()
        if not args.endswith(')'):
            err('Unmatched "("')
        args = args[:-1].strip()
        if args == '*': # * matches all indices, so does None
            args = args_raw = None
        else:
            args_raw = args = tuple(normalize(a).strip() for a in args.split(','))
            args = tuple(int(a) if a.isdigit()
                         else loop_vars.get(a, a)
                         for a in args)
            for a in args:
                if a!='*' and isinstance(a, str) and key!='category':
                    err('Loop variable "%s" is not active' % a)
    else:
        key = expr
        args = args_raw = None

    if args is None:  # No () in form.  Note that {x} is different than {x()}
        # Forms with no args
        # Builtins
        val = builtins.get(key)
        if val is not None:
            if callable(val):
                val = val()
            return (bool(val), val, None)

        # Loop variables, eg {BC} # TODO {Species(P)}
        nkey = normalize(key)
        lv = loop_vars.get(nkey)
        if lv:
            if nkey == 'category':
                sval = str(lv).title()
                sval = sval.replace('Udf', 'UDF')
                sval = sval.replace('And', 'and')
            else:
                sval = str(lv)
            return (True, sval, None)

        if key.startswith('category'):
            arg = key[8:].strip()
            cat = get_category(arg)
            ret1 = []
            ret2 = []
            for (k, a) in all_keys:
                if cat in keyword_doc.get(k, {}).get('category', 'unknown'):
                    ret1.append(proj.get(k, a).line())
                    ret2.append((k, a))
            return (bool(ret1), sorted(ret1), ret2)

        # "*" in key matches everything, regardless of args, eg BC_*_S
        if '*' in key:
            pat = re.compile(key.replace('*', '.*'), re.IGNORECASE)
            ret1 = []
            ret2 = []
            for (k, a) in all_keys:
                if pat.match(k):
                    ret1.append(proj.get(k, a).line())
                    ret2.append((k,a))
            return (bool(ret1), sorted(ret1), ret2)

        # {Phases}, {BCs} etc.
        # I will not require anyone to type 'speciess'!
        if key.endswith('s'):
            sing = key if key=='species' else 'category' if key =='categories' else key[:-1]
            if sing in arg_types or sing=='category':
                lv = normalize(sing)
                if lv in loop_ranges:
                    return (True, str(len(loop_ranges[lv])), None)
                else:
                    return (False, '', None)


    # Forms with zero or more args
    if args is None:
        args = args_raw = ()
    # region name, phase name, etc
    func = name_funcs.get(key)
    if func:
        name = func(args, args_raw)
        return (bool(name), name or '', None)

    # Catch arg type mismatches early
    if args and key in keyword_args:
        if not args_typematch(keyword_args.get(key,()),
                              args_raw):
            err('Keyword "%s": expected "%s", got "%s"' %
                (key, keyword_args.get(key,()), args_raw))

    # Existing key
    if (key, args) in all_keys:
        k = proj.get(key, args)
        if k is None: # looping over unset value (?)
            val = None
            sval = ''
        else:
            val = k.value
            sval = k.line()
        return (bool(val), sval, ((key, args),))

    # Wildcard in key or args
    if '*' in key or '*' in args:
        pat = re.compile(key.replace('*', '.*'), re.IGNORECASE)
        ret1= []
        ret2 = []
        for (k, a) in all_keys:
            if (pat.match(k)
                    and args_match(args,a)
                    and args_typematch(keyword_args.get(k,()),
                                       args_raw)):
                ret1.append(proj.get(k, a).line())
                ret2.append((k,a))
        return (bool(ret1), sorted(ret1), ret2)


    # Key not present in input, but use_defaults enabled
    if use_default:
        # Check if key present in project, even if already matched
        k = proj.get(key, args)
        if k:
            val = k.value
            return (bool(val), k.line(), None)
        val = defaults.get(key)
        if val is not None:
            return (bool(val), Keyword(key, val, args=list(args)).line(), None)
        # Need to disable this for new 'if' logic, since some keys don't have
        #  defaults
        #err('No default value for keyword "%s"' % key)

    # No matches
    return (False, '', ())


def expand(expr):
    if expr == '_':
        repl, to_remove = last_match
    else:
        use_default = expr.endswith('!')
        if use_default:
            expr = expr[:-1]
        bval, repl, to_remove = _eval(expr, use_default)
    repl = repl or ''
    if not isinstance(repl, (list, tuple)):
        repl = [repl]
    if to_remove and if_val and not empty_loop:
        for k in to_remove or []:
            try:
                all_keys.remove(k)
            except KeyError:
                # Key may have been matched by more than one pattern
                pass
    return repl


def region_name(args, args_raw):
    if len(args) != 1:
        err('region_name: expected 1 arg, got "%s"' % str(args_raw))
    arg = args[0]
    arg_raw = args_raw[0]

    # Decide what kind of region name based on the argument
    region_type = normalize(arg_raw)
    region_key = region_type + '_regions'
    region_json = proj.mfix_gui_comments.get(region_key)
    if region_json:
        region_list = json.loads(region_json)
    else:
        region_list = []

    for (indices, regions) in region_list:
        # May be a list (BC) or single integer (Monitor)
        if isinstance(indices, int):
            indices = [indices]
            regions = [regions]
        for (i,r) in zip(indices, regions):
            if i == arg:
                return r

def phase_name(args, args_raw):
    if len(args) != 1:
        err('phase_name: expected 1 arg, got "%s"' % str(args_raw))
    arg = args[0]
    arg_raw = args_raw[0]

    if not arg_raw.isdigit() and arg_raw != 'phase':
        err('phase_name: expected phase argument, got "%s"' % arg_raw)

    if arg == 0:
        key = 'fluid_phase_name'
    else:
        key = 'solids_phase_name(%s)' % arg
    name = proj.mfix_gui_comments.get(key)
    return name

def scalar_name(args, args_raw):
    if len(args) != 1:
        err('scalar_name: expected 1 arg, got "%s"' % str(args_raw))
    arg = args[0]
    arg_raw = args_raw[0]

    if not arg_raw.isdigit() and arg_raw != 'scalar':
        err('scalar_name: expected scalar argument, got "%s"' % arg_raw)
    key = 'scalar_name(%s)' % arg
    name = proj.mfix_gui_comments.get(key)
    return name

def species_name(args, args_raw):
    if len(args) == 1:
        phase = 0
        if not args_raw[0].isdigit() and args_raw[0] != 'species':
            err('species_name: expected species argument, got "%s"' % args_raw[0])
        species = args[0]
    elif len(args) == 2:
        if not args_raw[0].isdigit() and args_raw[0] != 'phase':
            err('species_name: expected phase argument, got "%s"' % args_raw[0])
        if not args_raw[1].isdigit() and args_raw[1] != 'species':
            err('species_name: expected species argument, got "%s"' % args_raw[1])
        phase, species = args
    else:
        err('species_name:  expected 1 or 2 args, got "%s"' % str(args_raw))
    if phase == 0:
        key = 'species_alias_g'
        args = (species,)
    else:
        key = 'species_alias_s'
        args = (phase, species)

    k = proj.get(key, args)
    return None if k is None else k.value



name_funcs = {'scalar_name': scalar_name,
              'phase_name': phase_name,
              'region_name': region_name,
              'species_name': species_name}

# TODO implement 'reaction_name'

def is_expr(s):
    return s.startswith('{') and s.endswith('}')

def join(seq):
    return [''.join(x) for x in product(*seq)]

def is_blank(line):
    s = line.strip()
    return s in ('', '#', '#!', '!')

def is_comment(line):
    s = line.strip()
    return (not s) or s[0] in ('', '#', '#!', '!')


def squeeze_blanks(lines):
    ret = []
    skip = 0
    for l in lines:
        l = l.rstrip()
        if is_blank(l):
            if skip > 1:
                continue
            skip += 1
        else:
            skip = 0
        ret.append(l)
    return ret


def project_to_string(p, template='standard'):
    global lineno
    global proj
    global keyword_doc
    global categories
    global defaults

    # Pick this up again from the current project, because it might have extra
    # user-defined keys
    if p.keyword_doc:
        keyword_doc = p.keyword_doc # When running standalone, P has no keyword_doc
    categories = set(v.get('category') for v in keyword_doc.values())
    defaults = dict((k, v.get('initpython')) for (k,v) in keyword_doc.items())

    proj = p

    load_template(template)
    output_lines = []
    align = False

    for k in proj.keywordItems():
        if k.key.startswith(('monitor_', 'part_out_', 'part_in_', 'vtk_')):
            # Skip false values for booleans which are false by default
            if k.value is False is keyword_doc.get(k.key, {}).get('initpython'):
                continue
        all_keys.add((k.key, tuple(k.args)))

    # Find valid loop ranges
    for (key, args) in all_keys:
        key = key.lower()
        doc = keyword_doc.get(key)
        cat = 'unknown'
        if doc:
            cat = doc.get('category')
        if 'category' not in loop_ranges:
            loop_ranges['category'] = set()
        loop_ranges['category'].add(cat)

        types = keyword_args.get(key)
        if types is None:
            # Undocumented key
            warn('Undocumented key "%s"' % key)
            continue
        for (t,a) in zip(types, args):
            if t=='species':
                if 'phase' in types:
                    # solid
                    phase = args[types.index('phase')]
                else:
                    phase = 0
                t = (t, phase)
            if t not in loop_ranges:
                loop_ranges[t] = {a}
            else:
                loop_ranges[t].add(a)
    # Do not loop over fluid phase
    if 0 in loop_ranges.get('phase', []):
        loop_ranges['phase'].remove(0)

    # Make sure loop range for 'rate' goes at least from 1 to 'nrr'
    nrr = proj.get('nrr')
    if nrr:
        if 'rate' not in loop_ranges:
            loop_ranges['rate'] = set()
        for i in range(1, nrr.value+1):
            loop_ranges['rate'].add(i)

    # Scan template looking for {} expressions
    pat = re.compile(r'(\{[^}]*})')
    lineno = 0
    eof_lineno = len(template_lines)
    while lineno < eof_lineno:
        line = template_lines[lineno]
        lineno += 1

        if not line: # Pass through blank lines or they will be swallowed
            if if_val and not empty_loop:
                output_lines.append(line)
            continue

        expansion = []
        n_expr = 0  #Number of { } expression on line

        for match in pat.split(line):
            if match == '':
                continue

            if not is_expr(match):
                if if_val and not empty_loop:
                    expansion.append((match,))
                continue

            n_expr += 1
            text = match[1:-1] # Remove { }
            tok = text.strip().split()

            if not tok:
                err('Empty {}')

            # {align on|off}  # TODO enable/disable in sections of file.  For now it's global
            if tok[0] == 'align':
                if len(tok) != 2:
                    err('Expected "on" or "off" after "{align"')
                if tok[1] == 'on':
                    align = True
                elif tok[1] == 'off':
                    align = False
                else:
                    err('Expected "on" or "off" after "{align"')
            # {loop ...}
            elif tok[0] == 'loop':
                if len(tok) != 2:
                    err('Expected variable name after "{loop"')
                loop_var = tok[1].lower()
                start_loop(loop_var, lineno)

            # {if ... }
            elif tok[0] == 'if':
                if len(tok) < 2:
                    err('Expected condition after "{if"')
                cond = ' '.join(tok[1:])
                start_if(cond, lineno)

            # {end ...}
            elif tok[0] == 'end':
                if len(tok) < 2 or tok[1].lower() not in ('loop', 'if'):
                    err('Expected "loop" or "if" after "end"')
                if tok[1].lower() == 'loop':
                    if len(tok) == 3:
                        end_var = normalize(tok[2])
                    else:
                        end_var = None
                    if len(tok) > 3:
                        err('Unexpected input after {end')
                    x = end_loop(end_var)
                    lineno = x
                elif tok[1].lower() == 'if':
                    end_if(' '.join(tok))

            # {everything else}
            else:
                # Expand everything, even in a failed if or empty loop, to catch more errors
                repl = expand(' '.join(tok))
                # right-padding.
                width = len(text) if text.endswith(' ') else None
                if width is not None:
                    repl = [r + ' '*(width-len(r)) for r in repl]
                if if_val and not empty_loop:
                    expansion.append(repl)
        if not expansion:
            continue
        lines = join(expansion)
        if n_expr > 0 and all(is_blank(l) for l in lines):
            continue
        output_lines.extend(lines)

    if stack:
        lineno = stack[-1][-1]
        err("No matching {end")

    if all_keys:
        msg = "%s keys unmatched in template: %s" % (
            len(all_keys), ', '.join(sorted(format_key_with_args(k, a or None)
                                            for (k,a) in all_keys)))
        if len(msg) > 500:
            msg = msg[:500] + '...'
        err(msg)

    if align:
        align_equals(output_lines)

    output_lines.extend(chemistry_lines())
    output_lines.extend(mfix_gui_comment_lines())
    output_lines.extend(thermo_data_lines())
    output_lines = [line.rstrip() for line in output_lines]
    if output_lines:
        output_lines.append('') # force final newline
    return '\n'.join(squeeze_blanks(output_lines))


def align_equals(lines):
    # Find blocks of lines which contain an equal sign, and make the equal signs line up
    # Typically there will not be more than one '=' on a line but we only care about the first one
    # Operates in-place
    idx = [line.find('=') for line in lines]
    starts = [i for i,x in enumerate(idx) if x>0 and (i==0 or idx[i-1]<0)]
    ends = [i for i,x in enumerate(idx) if x>0 and (i==len(idx)-1 or idx[i+1]<0)]
    for start,end in zip(starts, ends):
        m = max(idx[start:end+1])
        for i in range(start, end+1):
            lines[i] = lines[i].replace('=', (m-idx[i])*' ' + '=', 1)


def chemistry_lines():
    rxns = []
    des_rxns = []
    # defer the decision whether a reaction is DES or not until as late as possible
    #  since solids models can change
    for (name, reaction) in proj.reactions.items():
        if reaction.get('chem_eq') is None: # Don't save incompletely-defined reactions
            continue
        des = any(proj.get_value('solids_model', default='TFM', args=[p]) != 'TFM'
                  for p in reaction.get('phases', []))
        if des:
            des_rxns.append(name)
        else:
            rxns.append(name)

    if rxns or des_rxns:
        yield '\n# Chemical reaction section'
    if rxns:
        yield '@(RXNS)'
        for name in rxns:
            for line in proj.format_reaction(name):
                yield line
        yield '@(END)'
    if des_rxns:
        yield '@(DES_RXNS)'
        for name in des_rxns:
            for line in proj.format_reaction(name):
                yield line
        yield '@(DES_END)'


def mfix_gui_comment_lines():
    if proj.mfix_gui_comments: # Special comment block to hold non-keyword gui params
        yield '\n# MFIX-GUI section'

        for (key, val) in proj.mfix_gui_comments.items():
            sval = str(val)
            keylen = len(key)
            # Make lines no longer than 80 chars, including #!MFIX-GUI prefix (11 chars)
            sval, rest = break_string(sval, 66-keylen) # 3 spaces for ' = ',
            yield '#!MFIX-GUI %s = %s' % (key, sval)
            while rest: # continuation lines
                sval, rest = break_string(rest, 68) # 1 space for continuation
                yield '#!MFIX-GUI  %s' % sval

def thermo_data_lines():
    if proj.thermo_data:
        yield '\nTHERMO DATA'
        for key in sorted(proj.thermo_data.keys()):
            for line in proj.thermo_data[key]:
                yield line.rstrip()
            yield '' # blank line between entries

def convert_tutorials():
    import glob
    from mfixgui.project import Project
    p_path = os.path.join(os.pardir, 'tutorials/*/*/*.mfx')
    print('convert', p_path)
    for fname in glob.glob(p_path):
        print(fname)
        p = Project()
        p.parsemfixdat(fname)
        with open(fname, "w", encoding="utf-8") as f:
            f.write(project_to_string(p))

def main(argv):
    from mfixgui.project import Project

    prog, args = argv[0], argv[1:]

    def Usage():
        sys.stderr.write("Usage: %s [-t template] input [output]\n" % prog)
        sys.stderr.write(" default template: %s\n" % template_file)
        sys.stderr.write(" use output '-' to write to stdout\n")
        return False

    if not args:
        return Usage()

    template = 'standard'
    if args[0].startswith('-'):
        if args[0] != '-t' or len(args) < 2:
            return Usage()
        template, args = args[1], args[2:]
        if not os.path.exists(template):
            tmp = os.path.join(DIR, 'output_templates', template)
            if os.path.exists(tmp):
                template = tmp
            else:
                sys.stderr.write('Template file "%s" not found\n' % template)
                return False

    if not args:
        return Usage()

    infile, args = args[0], args[1:]

    if not os.path.exists(infile):
        sys.stderr.write('MFiX file "%s" not found\n' % infile)
        return False

    if args:
        if len(args) > 1:
            return Usage()
        base, args = args[0], args[1:]
    else:
        base = os.path.basename(infile) + '.new'
    if args:
        return Usage()

    outfile = base

    suffix = 0
    while outfile != '-' and os.path.exists(outfile):
        suffix += 1
        outfile = '%s-%s' % (base, suffix)

    if outfile == '-':
        out = sys.stdout
    else:
        print('Writing to %s' % outfile)
        out = open(outfile, 'w')

    p = Project()
    p.parsemfixdat(infile)
    if not p.keywordItems():
        print('No keys found in "%s"' % infile)
        return False

    out.write(project_to_string(p, template=template))
    return True

if __name__ == '__main__':
    sys.exit(main(sys.argv) - 1)
