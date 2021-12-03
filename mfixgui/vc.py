#!/usr/bin/env python

"""Version control for MFiX projects"""

import os
import git
import time
import re

repo = None

html = '<html><body><pre>%s</pre></html></body>'

textbrowser = None

def plural(n, word):
    fmt = "%d %s" if n == 1 else "%d %ss"
    return fmt % (n, word)

def anchor(t):
    return("<a href='changes/%s'>changes</a>" % t)

def gen_report():
    global repo
    repo = git.Repo.init(path=os.getcwd())
    tags = repo.git.tag(sort='creatordate').split('\n')
    now = time.time()
    def ago(t):
        tdiff = int(now - t)
        if tdiff < 0:
            return '%s seconds in the future(?)' % -tdiff
        elif tdiff < 60:
            return plural(tdiff, 'second') + ' ago'
        elif tdiff < 3600:
            return plural(tdiff//60, 'minute') + ' ago'
        elif tdiff < 3600*12: # FIXME - today
            return time.strftime('%H:%M:%S', time.localtime(t))
        else:
            return time.ctime(t)
    header = "      Revision            Time               Author"
    header2= "      --------            ----               ------"
    text = [header, header2]
    for t in reversed(tags):
        c = repo.tags[t].commit
        line = ('% 6s % 6s  % 24s     %s' %
                (t, anchor(t), ago(c.committed_date), c.author.name))
        text.append(line)

    return ('\n'.join(text))


report_text = ''
def update(tb):
    global textbrowser
    global report_text
    textbrowser = tb # from mfixgui
    report_text = html % gen_report()
    textbrowser.setText(report_text)


def diff(tag):
    tags = repo.git.tag(sort='creatordate').splitlines()
    if tag not in tags:
        return('Unknown version %s' % tag)
    if tag == tags[0]:
        return("No previous version to compare")
    prev_tag = tags[tags.index(tag)-1]


    # Word diff
    use_word_diff = True
    if use_word_diff:
        lines = repo.git.diff('-U0', '--word-diff=color', prev_tag, tag).splitlines()
        lines = lines[5:] # Skip header
        ret = []
        for line in lines:
            line0 = line
            if '\x1b[36m' in line:
                line = line[:line.index('\x1b[m')+3]
            line = re.sub('\x1b\\[1m(.*?)\x1b\\[m',
                          r'<span style=color:blue>\1</span>', line)
            line = re.sub('\x1b\\[36m(.*?)\x1b\\[m',
                          r'<i><span style=color:grey>\1</span></i>', line)
            line = re.sub('\x1b\\[31m(.*?)\x1b\\[m',
                          r'<span style=text-decoration:line-through;color:red>\1</span>', line)
            line = re.sub('\x1b\\[32m(.*?)\x1b\\[m',
                          r'<span style=text-decoration:underline;color:green>\1</span>', line)


            #line = line.replace('\x1b[1m','<font color=blue>')
            #line = line.replace('\x1b[36m', '<font color=gray>')
            #line = line.replace('\x1b[31m', '<font color=red>')
            #line = line.replace('\x1b[32m', '<font color=green>')
            #line = line.replace('\x1b[m', ' </font>')
            ret.append(line)

        return '\n'.join(ret)
    # Side by side diff
    else:
        lines = repo.git.diff('-U0', prev_tag, tag).splitlines()
        lines = lines[5:]  # throw away header
        ret = []
        block = []
        a_lines = []
        b_lines = []
        def flush():
            na, nb = len(a_lines), len(b_lines)
            if na > nb:
                b_lines.extend((nb-na)*[''])
            elif nb > na:
                a_lines.extend((na-nb)*[''])
            block = ['%-75s %-75s' % ab for ab in zip(a_lines, b_lines)]
            ret.extend(block)
            ret.append('')
            a_lines.clear()
            b_lines.clear()
        for line in lines:
            if not line:
                continue
            c = line[0]
            if c == '@':
                flush()
            elif c == '-':
                a_lines.append(line[1:])
            elif c == '+':
                b_lines.append(line[1:])
        flush()
    return '\n'.join(ret)


def handle_click(url):
    url = url.toString()
    parts = url.split('/')
    if parts[0] == 'changes':
        tag = parts[1]
    elif parts[0] == 'collapse':
        textbrowser.setText(report_text) # use "back"?
        return
    else:
        raise ValueError(url)
    lines = report_text.splitlines()
    a = anchor(tag)
    for lineno, line in enumerate(lines):
        if a in line:
            lines[lineno] = line.replace(a, "<a href='collapse'>collapse</a>")
            break
    lines.insert(lineno+1, '\n'+diff(tag)+'\n')
    textbrowser.setText('\n'.join(lines))
