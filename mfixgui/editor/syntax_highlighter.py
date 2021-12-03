"""
Collection of syntax highlighters. The majority of this code was inspired/pulled
from the Spyder IDE:
https://github.com/spyder-ide/spyder/blob/master/spyder/utils/syntaxhighlighters.py
"""

from qtpy.QtCore import Qt
from qtpy.QtGui import QFont, QSyntaxHighlighter, QTextCharFormat

import re
import keyword
import builtins

from mfixgui.editor.constants import DEFAULT_COLOR_SCHEME
from mfixgui.namelistparser import getKeywordDoc


def get_syntax_highlighter(language):
    return SYNTAX_HIGHLIGHTERS.get(language, BaseSH)


def make_any(name, alternates):
    "Return a named group pattern matching list of alternates."
    return "(?P<%s>" % name + "|".join(alternates) + ")"


def to_text_string(s):
    return s


class TextBlockHelper(object):
    """
    Helps retrieving the various part of the user state bitmask.
    This helper should be used to replace calls to
    ``QTextBlock.setUserState``/``QTextBlock.getUserState`` as well as
    ``QSyntaxHighlighter.setCurrentBlockState``/
    ``QSyntaxHighlighter.currentBlockState`` and
    ``QSyntaxHighlighter.previousBlockState``.
    The bitmask is made up of the following fields:
        - bit0 -> bit26: User state (for syntax highlighting)
        - bit26: fold trigger state
        - bit27-bit29: fold level (8 level max)
        - bit30: fold trigger flag
        - bit0 -> bit15: 16 bits for syntax highlighter user state (
          for syntax highlighting)
        - bit16-bit25: 10 bits for the fold level (1024 levels)
        - bit26: 1 bit for the fold trigger flag (trigger or not trigger)
        - bit27: 1 bit for the fold trigger state (expanded/collapsed)
    """
    @staticmethod
    def get_state(block):
        """
        Gets the user state, generally used for syntax highlighting.
        :param block: block to access
        :return: The block state
        """
        if block is None:
            return -1
        state = block.userState()
        if state == -1:
            return state
        return state & 0x0000FFFF

    @staticmethod
    def set_state(block, state):
        """
        Sets the user state, generally used for syntax highlighting.
        :param block: block to modify
        :param state: new state value.
        :return:
        """
        if block is None:
            return
        user_state = block.userState()
        if user_state == -1:
            user_state = 0
        higher_part = user_state & 0x7FFF0000
        state &= 0x0000FFFF
        state |= higher_part
        block.setUserState(state)


class BaseSH(QSyntaxHighlighter):
    """Base Syntax Highlighter Class"""
    # Syntax highlighting rules:
    PROG = None
    BLANKPROG = re.compile(r"\s+")
    # Syntax highlighting states (from one text block to another):
    NORMAL = 0
    # Syntax highlighting parameters.
    BLANK_ALPHA_FACTOR = 0.31
    # keywords and builtins for completer
    keywords = []
    builtins = []
    def __init__(self, parent, font=None, color_scheme=DEFAULT_COLOR_SCHEME):
        QSyntaxHighlighter.__init__(self, parent)

        self.font = font
        self.formats = {}
        self.set_color_scheme(color_scheme)

    def set_color_scheme(self, colors):
        self.color_scheme = colors
        self.formats = {}
        base_format = QTextCharFormat()
        if self.font is not None:
            base_format.setFont(self.font)
        for name, color in colors.items():
            format = QTextCharFormat(base_format)
            format.setForeground(color)
            format.setBackground(colors.get('background'))
            self.formats[name] = format

    def highlightBlock(self, text):
        pass


class GenericSH(BaseSH):
    """Generic Syntax Highlighter"""
    # Syntax highlighting rules:
    PROG = None  # to be redefined in child classes

    def highlightBlock(self, text):
        """Implement highlight using a regex."""
        text = to_text_string(text)
        self.setFormat(0, len(text), self.formats["normal"])

        match = self.PROG.search(text)
        index = 0
        while match:
            for key, value in list(match.groupdict().items()):
                if value:
                    start, end = match.span(key)
                    index += end-start
                    self.setFormat(start, end-start, self.formats[key])

            match = self.PROG.search(text, match.end())


#==============================================================================
# Fortran Syntax Highlighter
# extracted from spyder
# https://github.com/spyder-ide/spyder/blob/master/spyder/utils/syntaxhighlighters.py
#==============================================================================
FORTRAN_kwstr = 'access action advance allocatable allocate apostrophe assign assignment associate asynchronous backspace bind blank blockdata call case character class close common complex contains continue cycle data deallocate decimal delim default dimension direct do dowhile double doubleprecision else elseif elsewhere encoding end endassociate endblockdata enddo endfile endforall endfunction endif endinterface endmodule endprogram endselect endsubroutine endtype endwhere entry eor equivalence err errmsg exist exit external file flush fmt forall form format formatted function go goto id if implicit in include inout integer inquire intent interface intrinsic iomsg iolength iostat kind len logical module name named namelist nextrec nml none nullify number only open opened operator optional out pad parameter pass pause pending pointer pos position precision print private program protected public quote read readwrite real rec recl recursive result return rewind save select selectcase selecttype sequential sign size stat status stop stream subroutine target then to type unformatted unit use value volatile wait where while write'
FORTRAN_bistr1 = 'abs achar acos acosd adjustl adjustr aimag aimax0 aimin0 aint ajmax0 ajmin0 akmax0 akmin0 all allocated alog alog10 amax0 amax1 amin0 amin1 amod anint any asin asind associated atan atan2 atan2d atand bitest bitl bitlr bitrl bjtest bit_size bktest break btest cabs ccos cdabs cdcos cdexp cdlog cdsin cdsqrt ceiling cexp char clog cmplx conjg cos cosd cosh count cpu_time cshift csin csqrt dabs dacos dacosd dasin dasind datan datan2 datan2d datand date date_and_time dble dcmplx dconjg dcos dcosd dcosh dcotan ddim dexp dfloat dflotk dfloti dflotj digits dim dimag dint dlog dlog10 dmax1 dmin1 dmod dnint dot_product dprod dreal dsign dsin dsind dsinh dsqrt dtan dtand dtanh eoshift epsilon errsns exp exponent float floati floatj floatk floor fraction free huge iabs iachar iand ibclr ibits ibset ichar idate idim idint idnint ieor ifix iiabs iiand iibclr iibits iibset iidim iidint iidnnt iieor iifix iint iior iiqint iiqnnt iishft iishftc iisign ilen imax0 imax1 imin0 imin1 imod index inint inot int int1 int2 int4 int8 iqint iqnint ior ishft ishftc isign isnan izext jiand jibclr jibits jibset jidim jidint jidnnt jieor jifix jint jior jiqint jiqnnt jishft jishftc jisign jmax0 jmax1 jmin0 jmin1 jmod jnint jnot jzext kiabs kiand kibclr kibits kibset kidim kidint kidnnt kieor kifix kind kint kior kishft kishftc kisign kmax0 kmax1 kmin0 kmin1 kmod knint knot kzext lbound leadz len len_trim lenlge lge lgt lle llt log log10 logical lshift malloc matmul max max0 max1 maxexponent maxloc maxval merge min min0 min1 minexponent minloc minval mod modulo mvbits nearest nint not nworkers number_of_processors pack popcnt poppar precision present product radix random random_number random_seed range real repeat reshape rrspacing rshift scale scan secnds selected_int_kind selected_real_kind set_exponent shape sign sin sind sinh size sizeof sngl snglq spacing spread sqrt sum system_clock tan tand tanh tiny transfer transpose trim ubound unpack verify'
FORTRAN_bistr2 = 'cdabs cdcos cdexp cdlog cdsin cdsqrt cotan cotand dcmplx dconjg dcotan dcotand decode dimag dll_export dll_import doublecomplex dreal dvchk encode find flen flush getarg getcharqq getcl getdat getenv gettim hfix ibchng identifier imag int1 int2 int4 intc intrup invalop iostat_msg isha ishc ishl jfix lacfar locking locnear map nargs nbreak ndperr ndpexc offset ovefl peekcharqq precfill prompt qabs qacos qacosd qasin qasind qatan qatand qatan2 qcmplx qconjg qcos qcosd qcosh qdim qexp qext qextd qfloat qimag qlog qlog10 qmax1 qmin1 qmod qreal qsign qsin qsind qsinh qsqrt qtan qtand qtanh ran rand randu rewrite segment setdat settim system timer undfl unlock union val virtual volatile zabs zcos zexp zlog zsin zsqrt'

def make_fortran_patterns():
    "Strongly inspired from idlelib.ColorDelegator.make_pat"

    kw = r"\b" + make_any("keyword", FORTRAN_kwstr.split()) + r"\b"
    builtin = r"\b" + make_any("builtin", FORTRAN_bistr1.split()+FORTRAN_bistr2.split()) + r"\b"
    comment = make_any("comment", [r"\![^\n]*"])
    number = make_any("number",
                 [r"\b[+-]?[0-9]+[lL]?\b",
                  r"\b[+-]?0[xX][0-9A-Fa-f]+[lL]?\b",
                  r"\b[+-]?[0-9]+(?:\.[0-9]+)?(?:[eE][+-]?[0-9]+)?\b"])
    sqstring = r"(\b[rRuU])?'[^'\\\n]*(\\.[^'\\\n]*)*'?"
    dqstring = r'(\b[rRuU])?"[^"\\\n]*(\\.[^"\\\n]*)*"?'
    string = make_any("string", [sqstring, dqstring])
    return "|".join([kw, comment, string, number, builtin,
                     make_any("SYNC", [r"\n"])])

class FortranSH(BaseSH):
    """Fortran Syntax Highlighter"""
    # Syntax highlighting rules:
    PROG = re.compile(make_fortran_patterns(), re.S|re.I)
    IDPROG = re.compile(r"\s+(\w+)", re.S)
    # Syntax highlighting states (from one text block to another):
    NORMAL = 0
    # keywords/builtins for completion
    keywords = FORTRAN_kwstr.split()
    builtins = FORTRAN_bistr1.split()+FORTRAN_bistr2.split()
    def __init__(self, parent, font=None, color_scheme=DEFAULT_COLOR_SCHEME):
        BaseSH.__init__(self, parent, font, color_scheme)

    def highlightBlock(self, text):
        """Implement highlight specific for Fortran."""
        self.setFormat(0, len(text), self.formats["normal"])
        match = self.PROG.search(text)
        index = 0
        while match:
            for key, value in list(match.groupdict().items()):
                if value:
                    start, end = match.span(key)
                    index += end-start
                    self.setFormat(start, end-start, self.formats[key])
                    if value.lower() in ("subroutine", "module", "function"):
                        match1 = self.IDPROG.match(text, end)
                        if match1:
                            start1, end1 = match1.span(1)
                            self.setFormat(start1, end1-start1,
                                           self.formats["definition"])

            match = self.PROG.search(text, match.end())


class Fortran77SH(FortranSH):
    """Fortran 77 Syntax Highlighter"""
    def highlightBlock(self, text):
        """Implement highlight specific for Fortran77."""
        if text.startswith(("c", "C")):
            self.setFormat(0, len(text), self.formats["comment"])
        else:
            FortranSH.highlightBlock(self, text)
            self.setFormat(0, 5, self.formats["comment"])
            self.setFormat(73, max([73, len(text)]), self.formats["comment"])

#==============================================================================
# Python 3 Syntax Highlighter
# extracted from spyder
# https://github.com/spyder-ide/spyder/blob/master/spyder/utils/syntaxhighlighters.py
#==============================================================================

def make_python_patterns(additional_keywords=[], additional_builtins=[]):
    "Strongly inspired from idlelib.ColorDelegator.make_pat"
    kwlist = keyword.kwlist + additional_keywords
    builtinlist = [str(name) for name in dir(builtins)
                   if not name.startswith('_')] + additional_builtins
    repeated = set(kwlist) & set(builtinlist)
    for repeated_element in repeated:
        kwlist.remove(repeated_element)
    kw = r"\b" + make_any("keyword", kwlist) + r"\b"
    builtin = r"([^.'\"\\#]\b|^)" + make_any("builtin", builtinlist) + r"\b"
    comment = make_any("comment", [r"#[^\n]*"])
    instance = make_any("instance", [r"\bself\b",
                                r"\bcls\b",
                                (r"^\s*@([a-zA-Z_][a-zA-Z0-9_]*)"
                                     r"(\.[a-zA-Z_][a-zA-Z0-9_]*)*")])
    number_regex = [r"\b[+-]?[0-9]+[lLjJ]?\b",
                    r"\b[+-]?0[xX][0-9A-Fa-f]+[lL]?\b",
                    r"\b[+-]?0[oO][0-7]+[lL]?\b",
                    r"\b[+-]?0[bB][01]+[lL]?\b",
                    r"\b[+-]?[0-9]+(?:\.[0-9]+)?(?:[eE][+-]?[0-9]+)?[jJ]?\b"]
    prefix = "r|u|R|U|f|F|fr|Fr|fR|FR|rf|rF|Rf|RF|b|B|br|Br|bR|BR|rb|rB|Rb|RB"
    sqstring =     r"(\b(%s))?'[^'\\\n]*(\\.[^'\\\n]*)*'?" % prefix
    dqstring =     r'(\b(%s))?"[^"\\\n]*(\\.[^"\\\n]*)*"?' % prefix
    uf_sqstring =  r"(\b(%s))?'[^'\\\n]*(\\.[^'\\\n]*)*(\\)$(?!')$" % prefix
    uf_dqstring =  r'(\b(%s))?"[^"\\\n]*(\\.[^"\\\n]*)*(\\)$(?!")$' % prefix
    sq3string =    r"(\b(%s))?'''[^'\\]*((\\.|'(?!''))[^'\\]*)*(''')?" % prefix
    dq3string =    r'(\b(%s))?"""[^"\\]*((\\.|"(?!""))[^"\\]*)*(""")?' % prefix
    uf_sq3string = r"(\b(%s))?'''[^'\\]*((\\.|'(?!''))[^'\\]*)*(\\)?(?!''')$" \
                   % prefix
    uf_dq3string = r'(\b(%s))?"""[^"\\]*((\\.|"(?!""))[^"\\]*)*(\\)?(?!""")$' \
                   % prefix

    number_regex = [
            r"\b[+-]?0[xX](?:_?[0-9A-Fa-f])+[lL]?\b",
            r"\b[+-]?0[bB](?:_?[01])+[lL]?\b",
            r"\b[+-]?0[oO](?:_?[0-7])+[lL]?\b",
            r"\b[+-]?(?:0(?:_?0)*|[1-9](?:_?[0-9])*)[lL]?\b",
            r"\b((\.[0-9](?:_?[0-9])*')|\.[0-9](?:_?[0-9])*)"
            "([eE][+-]?[0-9](?:_?[0-9])*)?[jJ]?\b",
            r"\b[0-9](?:_?[0-9])*([eE][+-]?[0-9](?:_?[0-9])*)?[jJ]?\b",
            r"\b[0-9](?:_?[0-9])*[jJ]\b"]
    number = make_any("number", number_regex)

    string = make_any("string", [sq3string, dq3string, sqstring, dqstring])
    ufstring1 = make_any("uf_sqstring", [uf_sqstring])
    ufstring2 = make_any("uf_dqstring", [uf_dqstring])
    ufstring3 = make_any("uf_sq3string", [uf_sq3string])
    ufstring4 = make_any("uf_dq3string", [uf_dq3string])
    return "|".join([instance, kw, builtin, comment,
                     ufstring1, ufstring2, ufstring3, ufstring4, string,
                     number, make_any("SYNC", [r"\n"])])


class PythonSH(BaseSH):
    """Python Syntax Highlighter"""
    # Syntax highlighting rules:
    add_kw = ['async', 'await']
    PROG = re.compile(make_python_patterns(additional_keywords=add_kw), re.S)
    IDPROG = re.compile(r"\s+(\w+)", re.S)
    ASPROG = re.compile(r".*?\b(as)\b")
    # Syntax highlighting states (from one text block to another):
    (NORMAL, INSIDE_SQ3STRING, INSIDE_DQ3STRING,
     INSIDE_SQSTRING, INSIDE_DQSTRING) = list(range(5))
    # keywords and builtins for completion
    keywords =  keyword.kwlist
    builtins = [str(name) for name in dir(builtins) if not name.startswith('_')]
    def highlightBlock(self, text):
        """Implement specific highlight for Python."""
        prev_state = TextBlockHelper.get_state(self.currentBlock().previous())
        if prev_state == self.INSIDE_DQ3STRING:
            offset = -4
            text = r'""" '+text
        elif prev_state == self.INSIDE_SQ3STRING:
            offset = -4
            text = r"''' "+text
        elif prev_state == self.INSIDE_DQSTRING:
            offset = -2
            text = r'" '+text
        elif prev_state == self.INSIDE_SQSTRING:
            offset = -2
            text = r"' "+text
        else:
            offset = 0
            prev_state = self.NORMAL

        oedata = None
        import_stmt = None

        self.setFormat(0, len(text), self.formats["normal"])

        state = self.NORMAL
        match = self.PROG.search(text)
        while match:
            for key, value in list(match.groupdict().items()):
                if value:
                    start, end = match.span(key)
                    start = max([0, start+offset])
                    end = max([0, end+offset])
                    if key == "uf_sq3string":
                        self.setFormat(start, end-start,
                                       self.formats["string"])
                        state = self.INSIDE_SQ3STRING
                    elif key == "uf_dq3string":
                        self.setFormat(start, end-start,
                                       self.formats["string"])
                        state = self.INSIDE_DQ3STRING
                    elif key == "uf_sqstring":
                        self.setFormat(start, end-start,
                                       self.formats["string"])
                        state = self.INSIDE_SQSTRING
                    elif key == "uf_dqstring":
                        self.setFormat(start, end-start,
                                       self.formats["string"])
                        state = self.INSIDE_DQSTRING
                    else:
                        self.setFormat(start, end-start, self.formats[key])
                        if key == "keyword":
                            if value in ("def", "class"):
                                match1 = self.IDPROG.match(text, end)
                                if match1:
                                    start1, end1 = match1.span(1)
                                    self.setFormat(start1, end1-start1,
                                                   self.formats["definition"])
                            elif value == "import":
                                import_stmt = text.strip()
                                # color all the "as" words on same line, except
                                # if in a comment; cheap approximation to the
                                # truth
                                if '#' in text:
                                    endpos = text.index('#')
                                else:
                                    endpos = len(text)
                                while True:
                                    match1 = self.ASPROG.match(text, end,
                                                               endpos)
                                    if not match1:
                                        break
                                    start, end = match1.span(1)
                                    self.setFormat(start, end-start,
                                                   self.formats["keyword"])

            match = self.PROG.search(text, match.end())

        TextBlockHelper.set_state(self.currentBlock(), state)



#==============================================================================
# C/C++ syntax highlighter
# extracted from spyder
# https://github.com/spyder-ide/spyder/blob/master/spyder/utils/syntaxhighlighters.py
#==============================================================================
C_KEYWORDS1 = 'and and_eq bitand bitor break case catch const const_cast continue default delete do dynamic_cast else explicit export extern for friend goto if inline namespace new not not_eq operator or or_eq private protected public register reinterpret_cast return sizeof static static_cast switch template throw try typedef typeid typename union using virtual while xor xor_eq'
C_KEYWORDS2 = 'a addindex addtogroup anchor arg attention author b brief bug c class code date def defgroup deprecated dontinclude e em endcode endhtmlonly ifdef endif endlatexonly endlink endverbatim enum example exception f$ file fn hideinitializer htmlinclude htmlonly if image include ingroup internal invariant interface latexonly li line link mainpage name namespace nosubgrouping note overload p page par param post pre ref relates remarks return retval sa section see showinitializer since skip skipline subsection test throw todo typedef union until var verbatim verbinclude version warning weakgroup'
C_KEYWORDS3 = 'asm auto class compl false true volatile wchar_t'
C_TYPES = 'bool char double enum float int long mutable short signed struct unsigned void NULL'

def make_generic_c_patterns(keywords, builtins,
                            instance=None, define=None, comment=None):
    "Strongly inspired from idlelib.ColorDelegator.make_pat"
    kw = r"\b" + make_any("keyword", keywords.split()) + r"\b"
    builtin = r"\b" + make_any("builtin", builtins.split()+C_TYPES.split()) + r"\b"
    if comment is None:
        comment = make_any("comment", [r"//[^\n]*", r"\/\*(.*?)\*\/"])
    comment_start = make_any("comment_start", [r"\/\*"])
    comment_end = make_any("comment_end", [r"\*\/"])
    if instance is None:
        instance = make_any("instance", [r"\bthis\b"])
    number = make_any("number",
                 [r"\b[+-]?[0-9]+[lL]?\b",
                  r"\b[+-]?0[xX][0-9A-Fa-f]+[lL]?\b",
                  r"\b[+-]?[0-9]+(?:\.[0-9]+)?(?:[eE][+-]?[0-9]+)?\b"])
    sqstring = r"(\b[rRuU])?'[^'\\\n]*(\\.[^'\\\n]*)*'?"
    dqstring = r'(\b[rRuU])?"[^"\\\n]*(\\.[^"\\\n]*)*"?'
    string = make_any("string", [sqstring, dqstring])
    if define is None:
        define = make_any("define", [r"#[^\n]*"])
    return "|".join([instance, kw, comment, string, number,
                     comment_start, comment_end, builtin,
                     define, make_any("SYNC", [r"\n"])])

def make_cpp_patterns():
    return make_generic_c_patterns(C_KEYWORDS1+' '+C_KEYWORDS2, C_KEYWORDS3)

class CppSH(BaseSH):
    """C/C++ Syntax Highlighter"""
    # Syntax highlighting rules:
    PROG = re.compile(make_cpp_patterns(), re.S)
    # Syntax highlighting states (from one text block to another):
    NORMAL = 0
    INSIDE_COMMENT = 1
    # keywords and builtins for completer
    keywords = C_KEYWORDS1.split() + C_KEYWORDS2.split()
    builtins = C_KEYWORDS3.split() + C_TYPES.split()
    def highlightBlock(self, text):
        """Implement highlight specific for C/C++."""
        text = to_text_string(text)
        inside_comment = TextBlockHelper.get_state(self.currentBlock().previous()) == self.INSIDE_COMMENT
        self.setFormat(0, len(text),
                       self.formats["comment" if inside_comment else "normal"])

        match = self.PROG.search(text)
        index = 0
        while match:
            for key, value in list(match.groupdict().items()):
                if value:
                    start, end = match.span(key)
                    index += end-start
                    if key == "comment_start":
                        inside_comment = True
                        self.setFormat(start, len(text)-start,
                                       self.formats["comment"])
                    elif key == "comment_end":
                        inside_comment = False
                        self.setFormat(start, end-start,
                                       self.formats["comment"])
                    elif inside_comment:
                        self.setFormat(start, end-start,
                                       self.formats["comment"])
                    elif key == "define":
                        self.setFormat(start, end-start,
                                       self.formats["number"])
                    else:
                        self.setFormat(start, end-start, self.formats[key])

            match = self.PROG.search(text, match.end())

        last_state = self.INSIDE_COMMENT if inside_comment else self.NORMAL
        TextBlockHelper.set_state(self.currentBlock(), last_state)


#==============================================================================
# yaml highlighter
# extracted from spyder
# https://github.com/spyder-ide/spyder/blob/master/spyder/utils/syntaxhighlighters.py
#==============================================================================
def make_yaml_patterns():
    "Strongly inspired from sublime highlighter "
    kw = make_any("keyword", [r":|>|-|\||\[|\]|[A-Za-z][\w\s\-\_ ]+(?=:)"])
    links = make_any("normal", [r"#:[^\n]*"])
    comment = make_any("comment", [r"#[^\n]*"])
    number = make_any("number",
                 [r"\b[+-]?[0-9]+[lL]?\b",
                  r"\b[+-]?0[xX][0-9A-Fa-f]+[lL]?\b",
                  r"\b[+-]?[0-9]+(?:\.[0-9]+)?(?:[eE][+-]?[0-9]+)?\b"])
    sqstring = r"(\b[rRuU])?'[^'\\\n]*(\\.[^'\\\n]*)*'?"
    dqstring = r'(\b[rRuU])?"[^"\\\n]*(\\.[^"\\\n]*)*"?'
    string = make_any("string", [sqstring, dqstring])
    return "|".join([kw, string, number, links, comment,
                     make_any("SYNC", [r"\n"])])

class YamlSH(GenericSH):
    """yaml Syntax Highlighter"""
    # Syntax highlighting rules:
    PROG = re.compile(make_yaml_patterns(), re.S)



# =============================================================================
# Markdown highlighter
# extracted from spyder
# https://github.com/spyder-ide/spyder/blob/master/spyder/utils/syntaxhighlighters.py
# =============================================================================
def make_md_patterns():
    h1 = '^#[^#]+'
    h2 = '^##[^#]+'
    h3 = '^###[^#]+'
    h4 = '^####[^#]+'
    h5 = '^#####[^#]+'
    h6 = '^######[^#]+'

    titles = make_any('title', [h1, h2, h3, h4, h5, h6])

    html_tags = make_any("builtin", [r"<", r"[\?/]?>", r"(?<=<).*?(?=[ >])"])
    html_symbols = '&[^; ].+;'
    html_comment = '<!--.+-->'

    strikethrough = make_any('strikethrough', [r'(~~)(.*?)~~'])
    strong = make_any('strong', [r'(\*\*)(.*?)\*\*'])

    italic = r'(__)(.*?)__'
    emphasis = r'(//)(.*?)//'
    italic = make_any('italic', [italic, emphasis])

    # links - (links) after [] or links after []:
    link_html = (r'(?<=(\]\())[^\(\)]*(?=\))|'
                 '(<https?://[^>]+>)|'
                 '(<[^ >]+@[^ >]+>)')
    # link/image references - [] or ![]
    link = r'!?\[[^\[\]]*\]'
    links = make_any('link', [link_html, link])

    # blockquotes and lists -  > or - or * or 0.
    blockquotes = (r'(^>+.*)'
                   r'|(^(?:    |\t)*[0-9]+\. )'
                   r'|(^(?:    |\t)*- )'
                   r'|(^(?:    |\t)*\* )')
    # code
    code = make_any('code', ['^`{3,}.*$'])
    inline_code = make_any('inline_code', ['`[^`]*`'])

    # math - $$
    math = make_any('number', [r'^(?:\${2}).*$', html_symbols])

    comment = make_any('comment', [blockquotes, html_comment])

    return '|'.join([titles, comment, html_tags, math, links, italic, strong,
                     strikethrough, code, inline_code])


class MarkdownSH(BaseSH):
    """Markdown Syntax Highlighter"""
    # Syntax highlighting rules:
    PROG = re.compile(make_md_patterns(), re.S)
    NORMAL = 0
    CODE = 1

    def highlightBlock(self, text):
        text = to_text_string(text)
        previous_state = self.previousBlockState()

        if previous_state == self.CODE:
            self.setFormat(0, len(text), self.formats["code"])
        else:
            previous_state = self.NORMAL
            self.setFormat(0, len(text), self.formats["normal"])

        self.setCurrentBlockState(previous_state)

        match = self.PROG.search(text)
        match_count = 0
        n_characters = len(text)

        while match and match_count< n_characters:
            for key, value in list(match.groupdict().items()):
                start, end = match.span(key)

                if value:
                    previous_state = self.previousBlockState()

                    if previous_state == self.CODE:
                        if key == "code":
                            # Change to normal
                            self.setFormat(0, len(text),
                                           self.formats["normal"])
                            self.setCurrentBlockState(self.NORMAL)
                        else:
                            continue
                    else:
                        if key == "code":
                            # Change to code
                            self.setFormat(0, len(text), self.formats["code"])
                            self.setCurrentBlockState(self.CODE)
                            continue

                    self.setFormat(start, end - start, self.formats[key])

            match = self.PROG.search(text, match.end())
            match_count += 1

    def setup_formats(self, font=None):
        super(MarkdownSH, self).setup_formats(font)

        font = QTextCharFormat(self.formats['normal'])
        font.setFontItalic(True)
        self.formats['italic'] = font

        self.formats['strong'] = self.formats['definition']

        font = QTextCharFormat(self.formats['normal'])
        font.setFontStrikeOut(True)
        self.formats['strikethrough'] = font

        font = QTextCharFormat(self.formats['string'])
        font.setUnderlineStyle(True)
        self.formats['link'] = font

        self.formats['code'] = self.formats['string']
        self.formats['inline_code'] = self.formats['string']

        font = QTextCharFormat(self.formats['keyword'])
        font.setFontWeight(QFont.Bold)
        self.formats['title'] = font


#==============================================================================
# mfx highlighter
#==============================================================================
MFIX_keywords = list(getKeywordDoc().keys())
def make_mfx_patterns():
    kw = r"\b" + make_any("keyword", MFIX_keywords) + r"\b"
    comment = make_any("comment", [r"#[^\n]*", r"![^\n]*"])
    number = make_any("number",
                 [r"\.[truefalsTRUEFALS]+\.",
                  r"\b[+-]?[0-9]+[lL]?\b",
                  r"\b[+-]?0[xX][0-9A-Fa-f]+[lL]?\b",
                  r"\b[+-]?[0-9]+(?:\.[0-9]+)?(?:[eE][+-]?[0-9]+)?\b"])
    sqstring = r"(\b[rRuU])?'[^'\\\n]*(\\.[^'\\\n]*)*'?"
    dqstring = r'(\b[rRuU])?"[^"\\\n]*(\\.[^"\\\n]*)*"?'
    string = make_any("string", [sqstring, dqstring])
    titles = make_any('title', ['^##[^#]+', '^###[^#]+', '^####[^#]+', '^#####[^#]+', '^######[^#]+'])
    return "|".join([titles, string, number, comment, kw])

class MfxSH(GenericSH):
    """mfx/dat Syntax Highlighter"""
    # Syntax highlighting rules:
    PROG = re.compile(make_mfx_patterns(), re.S|re.I)
    # keywords and builtins for completer
    keywords = MFIX_keywords



SYNTAX_HIGHLIGHTERS = {
    'fortran77': FortranSH,
    'fortran':   FortranSH,
    'python':    PythonSH,
    'cpp':       CppSH,
    'yaml':      YamlSH,
    'markdown':  MarkdownSH,
    'mfix':      MfxSH,
}
