from html import escape
import logging
import re

from collections import namedtuple
from enum import Enum

from mfixgui.tools import parse_key_with_args, format_key_with_args



INFO = "MESSAGE"
WARNING = "WARNING"
ERROR = "ERROR"

LOGLEVEL_COLORS = {
    INFO: "blue",
    WARNING: "orangered",
    ERROR: "red"
}

RE_ERR_1000_BAD_KEY = re.compile(r"Error 1000: A keyword pair on line (\d+)")
RE_ERR_1000_MISSING_KEY = re.compile(r"Error 1000: Required input not specified: (.*)")
RE_ERR_1001_ILLEGAL_KEY = re.compile(r"Error 1001: Illegal or unknown input: (.*)")
RE_ERR_2000_BAD_LINE = re.compile(r"Error 2000: Unable to process line (\d+)")
RE_ERR_NUMBERED = re.compile(r"Error [0-9]{4}: (.*)")


class SolverLogMessage:
    def __init__(self, mfixgui, lines):
        self.lines = lines
        self.mfix_dat_line = None
        self.mfixgui = mfixgui
        self.parse_errors()
        self.level = self.get_level()
        self.location = None # Location in mfix console

    @property
    def text(self):
        """ Return the text of the message (as HTML)"""
        html_message = escape("\n".join([line.strip() for line in self.lines]))

        req = self.parse_required_keys()
        req_message = "<br/>" + self.required_inputs_message(req) if req else ""

        return f"<pre>{html_message}</pre>{req_message}"

    def get_level(self):
        if not self.lines:
            return None
        lower = self.lines[0].lower()
        return (INFO if lower.startswith('message')
            else WARNING if lower.startswith('warning')
            else ERROR if lower.startswith('error')
            else None)

    @property
    def color(self):
        return LOGLEVEL_COLORS.get(self.level, None)

    def keyword_error(self):
        if self.mfix_dat_line is not None:
            bad_line = self.mfix_dat_line
            keyword = bad_line.split("=", 1)[0].strip()
            return keyword, bad_line
        return None

    def parse_errors(self):
        datfile_lines = self.mfixgui.datfile()
        for line in self.lines:
            for rgx in RE_ERR_1000_BAD_KEY, RE_ERR_2000_BAD_LINE:
                match = rgx.search(line)
                if match:
                    lineno = int(match.group(1))

                    if 0 <= lineno - 1 < len(datfile_lines):
                        self.mfix_dat_line = datfile_lines[lineno - 1]
                        self.mfix_dat_lineno = lineno
                    else:
                        logging.getLogger(__name__).warn(
                            "Project file does not have line #{} in:  {}".format(lineno, line)
                        )

                    return

            match = RE_ERR_1001_ILLEGAL_KEY.search(line)
            if match:
                text, *_ = match.group(1).lower().split("=", 1)
                for (lineno, datfile_line) in enumerate(datfile_lines, 1):
                    is_text = isinstance(datfile_line, type(text))
                    if is_text and text in datfile_line.lower():
                        self.mfix_dat_line = datfile_line
                        self.mfix_dat_lineno = lineno + 1
                        return

    def parse_required_keys(self):
        # Now check for missing required inputs.
        required_keys = dict()
        for line in self.lines:
            match = RE_ERR_1000_MISSING_KEY.search(line)
            if match:
                text = match.group(1)
                key, args = parse_key_with_args(text)
                key = key.lower()
                # make sure it's still unset
                required_keys[key] = args
        return required_keys

    def required_inputs_message(self, required_keys):
        """ Additional formatting for bad keyword error messages """
        message = [
            "Required input%s not specified:" % ("s" if len(required_keys) > 1 else "")
        ]

        # TODO make key a hyperlink to keyword help
        for (key, args) in required_keys.items():
            if self.mfixgui.project.get_value(key, args=args) is None:
                kw_doc = self.mfixgui.keyword_doc.get(key, {}).get(
                    "description", "unknown"
                ).strip()
                kw_doc = kw_doc.split(".", 1)[0]  # Only keep first sentence
                kw_doc = kw_doc.split("\n", 1)[0]  # Remove existing formatting
                kw_doc = re.sub(r"\[.*\]", "", kw_doc)  # Remove [units], [default]
                formatted_key = format_key_with_args(key, args)
                message.append(f"<br/><b>{formatted_key}</b>: {kw_doc}")
        return "\n".join(message)

    def edit_keyword(self, newtext):
        """ replace keyword with value from text """
        kwerr = self.keyword_error()
        if kwerr is None:
            logging.getLogger(__name__).warning("edit_keyword called on non-keyword error message")
            return
        if not newtext:
            self.drop_keyword()  # User replaced it with blank
        tuples = self.mfixgui.project.parseKeywordLine(newtext)
        for (new_key, new_args, new_value) in tuples:
            if new_key:
                self.drop_keyword()  # Only drop the old one once we have a valid replacement
                self.mfixgui.update_keyword(new_key, new_value, args=new_args)
            else:
                self.mfixgui.print_internal("Error:  cannot parse %s" % newtext)


    def drop_keyword(self):
        """ remove keyword from the project """
        kwerr = self.keyword_error()
        if kwerr is None:
            logging.getLogger(__name__).warning("drop_keyword called on non-keyword error message")
            return
        key, _ = kwerr
        if "(" in key:  # This is a little crude, use parser instead?
            keyword, args = key.split("(", 1)
            args = args.split(")")[0]
            args = [int(x) for x in args.split(",")]
        else:
            keyword, args = key, None
        self.mfixgui.unset_keyword(keyword, args)



def parse_reinit_fail_message(line):
    if "Reinitialization failed" in line:
        return "Reinitialization failed."

    return None
