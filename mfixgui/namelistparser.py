#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
MFiX [Multiphase Flow with Interphase eXchanges] is a general-purpose
computer code developed at the National Energy Technology Laboratory
[NETL] for describing the hydrodynamics, heat transfer and chemical
reactions in fluid-solid systems.

Please visit: https://mfix.netl.doe.gov/

This python file contains a function to parse the MFiX init_namelist
files to extract documentation of the keywords.

@author: Justin Weber
"""

import os
import re
import xml.etree.ElementTree

from collections import OrderedDict
from glob import glob

import warnings

from mfixgui.tools import get_mfix_src
from mfixgui.arrows import LEFT_ARROW, RIGHT_ARROW, LEFT_RIGHT_ARROW

TO_DTYPE = {"REAL": "DP", "LOGICAL": "L", "INTEGER": "I", "CHARACTER": "C"}

FROM_DTYPE = {
    "DP": "double precision",
    "C": "character",
    "L": "logical",
    "I": "integer",
}

# Values from param_mod.f
DIMS = {
    # DES_MMAX
    'DIMENSION_BC': 500,
    # DIMENSION_CTRL
    'DIMENSION_IC': 500,
    'DIMENSION_IS': 500,
    'DIMENSION_PS': 5000,
    'DIMENSION_USR': 5,
    'DIMENSION_VTK': 100,
    'DIM_EQS': 10,
    'DIM_I': 5000,
    'DIM_J': 5000,
    'DIM_K': 5000,
    # DIM_LM
    'DIM_M': 10,
    'DIM_N_G': 100,
    'DIM_N_S': 100,
    'DIM_QUADRIC': 500,
    'DIM_SCALAR': 100,
    'N_SPX': 11,
}


keyword_block_regex = re.compile('(! *?<keyword.*?</keyword>)', re.DOTALL | re.M)
_cached_keyword_doc = None


def getKeywordDoc():
    """ build keyword dict from mfix source"""

    global _cached_keyword_doc
    if _cached_keyword_doc:
        return _cached_keyword_doc

    searchPathList = [namelist_f for namelist_f in
                      glob(os.path.join(get_mfix_src(), 'model', '**', '*.f'), recursive=True)
                      if 'namelist' in os.path.basename(namelist_f).lower()]

    sortedPathListTemp = searchPathList

    for _ in range(2):
        sortedPathList = []
        for fname in sortedPathListTemp:
            base = os.path.basename(fname)
            base_lower = base.lower()
            if base_lower == "init_namelist.f":
                sortedPathList.insert(0, fname)
            elif base_lower == "cartesian_grid_init_namelist.f":
                sortedPathList.insert(1, fname)
            elif base_lower == "des_init_namelist.f":
                sortedPathList.insert(2, fname)
            else:
                sortedPathList.append(fname)
        sortedPathListTemp = sortedPathList

    mfixKeywordDict = OrderedDict()

    for fname in sortedPathList:
        with open(fname, encoding="utf-8", errors="replace") as nl_file:
            nl_filecontent = nl_file.read()
            i = nl_filecontent.lower().find('implicit none')
            with warnings.catch_warnings() as ws:
                warnings.simplefilter("error") ### turn warnings into exceptions
                mfixKeywordDict.update(parse(nl_filecontent[i:]))

    _cached_keyword_doc = mfixKeywordDict
    return mfixKeywordDict


def parse(string):
    r"""
    Read mfix namelists to generate documentation.

    returns dictionary

    !<keyword category="category name" required="true/false"
    !    tfm="true/false" dem="true/false" pic="true/false"
    !                                    legacy="true/false">
    !  <description></description>
    !  <arg index="" id="" max="" min=""/>
    !  <dependent keyword="" value="DEFINED"/>
    !  <conflict keyword="" value="DEFINED"/>
    !  <valid value="" note="" alias=""/>
    !  <range min="" max="" />
      MFIX_KEYWORD = INIT_VALUE
    !</keyword>


    UNDEFINED <- double
    UNDEFINED_I <-integer
    UNDEFINED_C <- character
    ZERO <- double
    .true./.false. <- logical
    \d* <- integer
    \d*\.\d*[Dd]?\d? <- double

    """

    ret = OrderedDict()
    for match in keyword_block_regex.finditer(string):
        block = match[0]
        start_line = string[: match.start()].count('\n')
        for (pat, repl) in ((' & ', ' &amp; '),
                            ('<-->', LEFT_RIGHT_ARROW),
                            ('-->', RIGHT_ARROW),
                            ('<--', LEFT_ARROW)):

            block = block.replace(pat,repl)
        # Non-breaking spaces around arrows and '+', keep chem EQ's on one line
        for s in (LEFT_RIGHT_ARROW, RIGHT_ARROW, LEFT_ARROW, '+'):
            block = block.replace(' %s '%s, '&#160;%s&#160;'%s)
        try:
            key, kwdict = parse_block(block)
            if key:
                ret[key] = kwdict
        except Exception as e:
            if isinstance(e, xml.etree.ElementTree.ParseError):
                line, col = e.position
                txt = block.splitlines()[line-1] + '\n' + ' '*(1+col) + '^'
                msg = str(e)
                # Extract parenthesized term to simplify message
                if '(' in msg:
                    msg = msg.split('(',1)[-1].split(')',1)[0]
                # Fix line and column number in message because block did not
                # start on line 1
                if ': line' in msg:
                    msg = msg[:msg.index(': line')]

                warnings.warn(msg + ' at line %s, column %s'%(line+start_line, 1+col)
                              + '\n' +txt)
            else:
                raise

    return ret


def parse_block(keyword_block):
    """ keyword_block is the multiline string '<keyword ...> ... </keyword>"""

    # strip Fortran comments to find keyword and initval

    keyword, initval = None, None

    for line in keyword_block.splitlines():
        line = line.split("!")[0].strip()
        if line:
            tok = line.split('=')
            if len(tok) >= 2:
                keyword, initval = tok[0], tok[1]
                break
    if keyword is None or initval is None:
        return None, {}

    init = initval.strip(" -")
    is_negative = initval.strip(" ").startswith("-")
    keyword = keyword.strip()
    if "(" in keyword:
        keyword, shape = keyword.split("(", 1)
        shape = shape[:-1]  # Remove trailing paren
    else:
        shape = ""

    nargs = 1 + shape.count(",") if shape else 0
    #  Strip ! character from the first column,
    #  remove trailing ! comment after keyword=initval,
    #  and parse the rest as XML.

    tree = xml.etree.ElementTree.fromstring(
        os.linesep.join(
            [line.lstrip("!").split("!")[0] for line in keyword_block.splitlines()]
        )
    )

    validrange = {}
    for range_element in tree.findall("range"):
        for bound in ("min", "max"):
            mval = range_element.get(bound)
            if mval:
                validrange[bound] = mval
                try:
                    validrange[bound] = float(mval)
                except ValueError:
                    pass
                if mval.startswith('+'):
                    validrange['exclude_min'] = True

    dtype = tree.get('dtype')
    initpython = find_initpython(dtype, init, is_negative)

    def get_bool_attr(elem, attr):
        val = elem.get(attr)
        return bool(val and val.lower() != "false")

    def to_int(x):
        if x.isnumeric():
            return int(x)
        return DIMS.get(x, x)

    expected_args = OrderedDict({
        to_int(arg.get("index")): {
            "id": to_int(arg.get("id")),
            "min": to_int(arg.get("min")),
            "max": to_int(arg.get("max")),
        } for arg in tree.findall("arg")})

    expected_nargs = len(expected_args)
    if expected_nargs != nargs:
        raise TypeError("%s: expected %d args, found %s" % (
            keyword.upper(), expected_nargs, nargs))

    return keyword.lower(), {
        'init': init,
        'initpython': initpython,
        'dtype': TO_DTYPE[dtype],
        'description': unindented_description(keyword, tree),
        'category': tree.get('category').lower(),
        'required': get_bool_attr(tree, 'required'),
        'tfm': get_bool_attr(tree, 'tfm'),
        'dem': get_bool_attr(tree, 'dem'),
        'pic': get_bool_attr(tree, 'pic'),
        'legacy': get_bool_attr(tree, 'legacy'),
        'locked': get_bool_attr(tree, 'locked'),
        'args': expected_args,
        'dependents': OrderedDict({
            dep.get('keyword'):
            {
                'keyword': dep.get('keyword'),
                'value': dep.get('value')
            } for dep in tree.findall('dependent')}),
        'conflicts': OrderedDict({
            conflict.get('keyword'): {
                'keyword': conflict.get('keyword'),
                'value': conflict.get('value')
            } for conflict in tree.findall('conflict')}),
        'valids': OrderedDict({
            valid.get('value'): {
                'value': valid.get('value'),
                'note': ' '.join(valid.get('note').split()),
                'alias': valid.get('alias') or None,
            } for valid in tree.findall('valid')}),
        'validrange': validrange,
    }


def unindented_description(keyword, tree):
    r'''Return the description, unindented.

Input:
!  <description>
!    Multiline description will be
!    unindented by the indentation of
!    the first line.
!  </description>

Output:
"""Multiline description will be
unindented by the indentation of
the first non-blank line."""
'''
    desc = tree.find("description").text
    lines = desc.splitlines()
    indent = None
    for line in lines:
        if line:
            indent = len(line) - len(line.lstrip())
            break
    if indent is None:
        raise ValueError(f"Keyword description should not be blank for: {keyword}")
    ret = os.linesep.join(line[indent:] for line in lines)
    return ret

def find_initpython(dtype, init, is_negative=False):
    """ return a python value from the init """

    init_lower = init.lower()

    if dtype == "CHARACTER":
        if "undefined_c" in init_lower:
            initpython = None
        else:
            initpython = init.strip("'\"")  # remove extra quotes

    elif dtype == "INTEGER":
        initpython = (
            None
            if init_lower == "undefined_i"
            else (-int(init) if is_negative else int(init))
        )

    elif dtype == "LOGICAL":
        if init_lower in (".true.", ".false.", ".t.", ".f."):
            initpython = init_lower in (".t.", ".true.")
        else:
            raise ValueError(init)

    elif dtype == "REAL":
        initpython = {
            "zero": 0,
            "one": 1,
            "large_number": 1.0e32,
            "undefined": None,
        }.get(init_lower, False)
        if initpython is False:
            initpython = float(init_lower.replace("d", "e"))
        if is_negative and isinstance(initpython, float):
            initpython = -initpython
    else:
        raise TypeError(dtype)

    return initpython


def build_keywords_rst(directory=None):
    """ Creates doc/keywords.rst for Sphinx documentation """

    kw_doc = getKeywordDoc()
    categories = (
        "run control",
        "physical parameters",
        "numerical parameters",
        "geometry and discretization",
        "gas phase",
        "solids phase",
        "initial condition",
        "boundary condition",
        "internal surface",
        "point source",
        "output control",
        "udf control",
        "chemical reactions",
        "parallelization control",
        "batch queue environment",
        "two fluid model",
        "cartesian grid",
        "discrete element simulation",
        "discrete element model",
        "particle in cell",
        "questionable",
    )

    questionable_categories = (
        "direct quadrature method of moments (dqmom)",
        "questionable",
    )

    kw_cats = set(kw_doc[kw]["category"] for kw in kw_doc)
    assert set(kw_cats) == set(categories) | set(questionable_categories)

    for nl_cat in categories:

        catname = nl_cat.replace(" ", "_")
        if directory is None:
            directory = ""
        kw_filename = os.path.join(directory, f"{catname}_gen.rst")
        with open(kw_filename, "w", encoding="utf-8", errors="ignore") as rst:
            rst.write(
                """
.. role:: kwname
.. role:: kwtype
.. role:: required

"""
            )
            cats = questionable_categories if nl_cat == "questionable" else [nl_cat]
            kws = {
                key: kw_doc[key] for key in kw_doc if kw_doc[key]["category"] in cats
            }
            for keyword, keyword_data in kws.items():
                write_keyword_heading(rst, keyword, keyword_data)
                if keyword_data['valids']:
                    write_keyword_valids(rst, keyword_data)


def write_keyword_heading(rst, keyword, data):
    args = data['args']
    datatype = FROM_DTYPE[data['dtype']]
    if args:
        index_range = r"- :math:`%s \le %s \le %s`"
        indices = "\n".join(
            index_range % (v["min"], v["id"], str(v["max"]).replace("_", r"{\_}"))
            for v in args.values()
        )
        kwargs = "(" + ", ".join(a["id"] for a in args.values()) + ")"
    else:
        indices = ""
        kwargs = ""
    required = ":required:`required`" if data["required"] else ""
    kw_heading = f":kwname:`{keyword}{kwargs}`".upper()
    description = data["description"].replace("*", r"\*").replace("_", r"\_")
    flags = (
        (
            "Applies to Solids Model(s):  "
            + ", ".join(
                [f"**{flag.upper()}**" for flag in ("tfm", "dem") if data[flag]]
            )
        )
        if data["tfm"] or data["dem"]
        else ""
    )
    rst.write(
        f"""

.. _{keyword.upper()}:

{kw_heading}
{"~" * len(kw_heading)}

Data Type: :kwtype:`{datatype.upper()}`

{required}

{indices}

{flags}

{description}

"""
    )


def write_keyword_valids(rst, keyword_data):
    valids = keyword_data['valids']
    rst.write(
        """

.. list-table:: Valid Values
   :widths: 10, 5, 40
   :header-rows: 1

   * - Name
     - Default?
     - Description """
    )
    unquoted_init = keyword_data["init"].strip("'").strip('"')
    for valid, valid_data in valids.items():
        default = "|fisheye|" if unquoted_init.lower() == valid.lower() else ""
        validdata = valid_data["note"]
        rst.write(
            f"""
   * - ``{valid}``
     - {default}
     - {validdata} """
        )


def main():
    """ main function for testing """
    return parse(string="""
!<keyword dtype="CHARACTER" category="category name" required="true/false"
!                                     legacy="true/false">
!    <description>Test description
!               over multiple lines
!   </description>
!  <arg index="1" id="argID"
!        max="+Inf" min="-inf"/>
!  <arg index="" id="" max="" min=""/>
!   <dependent keyword="dependentKeyword"
!                        value="DEFINED"/>
!  <dependent keyword="" value="DEFINED"/>
!  <conflict keyword="conflictKeyword" value="DEFINED"/>
!   comment inside block
!  <conflict keyword="" value="DEFINED"/>
!  <valid value="VALIDvALUE" note="this is a
!      valid value" alias=""/>
!  <valid value="" note="" alias=""/>
!  <range min="" max="" />
!  <range min="0" max="2D0" />
MFIX_KEYWORD = INIT_VALUE !comment
!   </keyword>
"""
    )


if __name__ == "__main__":
    main()
