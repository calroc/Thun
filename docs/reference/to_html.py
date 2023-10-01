import xml.etree.ElementTree as ET
from collections import defaultdict
import hashlib, re, sys
sys.path.append('../../implementations/Python')
import markdown
import joy
from myhtml import HTML

TITLE = 'Thun Function Reference'

with open('Function-Reference.md') as f:
    md = f.read()
with open('../../implementations/defs.txt') as f:
    defs = f.read()

definitions = {
    name: body
    for name, body in (
        defi.split(None, 1)
        for defi in defs.splitlines()
        )
    if not name.startswith('_')
    }

def symbols_of(expression):
    if isinstance(expression, str):
        yield expression
        return
    if isinstance(expression, tuple) and expression:
        yield from symbols_of(expression[0])
        yield from symbols_of(expression[1])
        return

used_in = {
    name: sorted(set(symbols_of(joy.text_to_expression(definitions[name]))))
    for name in definitions
    }

used_by = defaultdict(list)
for name in used_in:
    for term in used_in[name]:
        used_by[term].append(name)
for el in used_by.values():
    el.sort()


def def_format(to, name):
    try:
        defi = definitions[name]
    except KeyError:
        return
    to = to.div(class_='definition')
    to.h3('Definition')
    to = to.blockquote
    start = 0
    for match in re.finditer('[^ [\]]+', defi):
        b, e = match.span()
        if b != start:
            to += defi[start:b]
        foo = match.group()
        anchor = anchors.get(foo, '')
        to.a(foo, href='#' + anchor)
        start = e
    end = defi[start:]
    if end:
        to += end



def foo(to, text):
    html = markdown.markdown(text, output_format="html5")
    html = '<div class="notes">' + html + '</div>'
    try:
        t = ET.fromstringlist([html])
    except:
        print(repr(html))
        raise
    to += t


basis_functions = set('''\
i dip branch loop
cons first rest stack swaack
dup swap pop clear'''.split())

k = re.split('^-+$', md, flags=re.MULTILINE)
#k = md.split('------------------------------------------------------------------------\n')
del k[0]
k = [section.splitlines() for section in k]

##s = set(range(len(k)))
##for i, section in enumerate(k):
##    for line in section:
##        if line.startswith('## '):
##            #print(i, line)
##            s.remove(i)  # cannot remove same i twice
##assert not s  # one header per section

def anchor_for(name):
    return (
        name
        if name.isalpha()
        else hashlib.sha256(name.encode()).hexdigest()
        )

anchors = {}
sections = {}
for i, section in enumerate(k):
    for line in section:
        if line.startswith('## '):
            name = line[3:].strip()
            sections[name] = section
            anchors[name] = anchor_for(name)
            section.remove(line)
            continue

combinators = set(
    name
    for (name, section) in sections.items()
    if 'Combinator' in section
    )
for name in combinators:
    sections[name].remove('Combinator')

crosslinks = {}
for name, section in sections.items():
    try:
        i = section.index('### Crosslinks')
    except ValueError:
        continue
    crosslinks[name] = list(filter(None, section[i + 1:]))
    del section[i:]

discussions = {}
for name, section in sections.items():
    try:
        i = section.index('### Discussion')
    except ValueError:
        continue
    discussions[name] = list(filter(None, section[i + 1:]))
    del section[i:]


def add_crosslinks(to, name):
    try:
        links = crosslinks[name]
    except KeyError:
        return
    to = to.div(class_='crosslinks')
    to.h3('See Also')
    first = True
    for link in links:
        if first:
            first = not first
        else:
            to += ' '
        match = re.match('\[(.+)\].*', link)
        if not match:
            print('!', link)
            continue
        link_to = match.group(1)
        anchor = anchors[link_to]
        to.a(link_to, href='#' + anchor, class_='func_name')


def add_discussion(to, name):
    try:
        discussion = discussions[name]
    except KeyError:
        return
    to = to.div(class_='discussion')
    to.h3('Discussion')
    to += ('\n'.join(discussion))


def add_backlinks(to, name):
    try:
        links = used_by[name]
    except KeyError:
        return
    if not links:
        return
    to = to.div(class_='backlinks')
    to.h3('Used By')
    first = True
    for link_to in links:
        if first:
            first = not first
        else:
            to += ' '
        anchor = anchors.get(link_to, '')
        to.a(link_to, href='#' + anchor, class_='func_name')


doc = HTML()

with doc.head as h:
    h.meta(charset='utf-8')
    h.title(TITLE)
    h.link(rel='stylesheet', href='/css/fonts.css')
    h.link(rel='stylesheet', href='/css/func_ref.css')

with doc.body as b:
    b.h1(TITLE)
    b.a('Home', href='/')
    b.p('Version -10.0.0')
    b.p('Each function, combinator, or definition should be documented here.')
    #b.hr
    ul = b.ul
    for name, section in sorted(sections.items()):
        ul.li.a(name, href='#' + anchor_for(name))
        ul += ' '
    for name, section in sorted(sections.items()):
        b.hr
        d = b.div
        anchor_id = anchor_for(name)
        title = d.h2(name, id=anchor_id, class_='func_name')
        title += ' '
        title.a('¶', href='#' + anchor_id, class_='self_link')
        with d.p as tags:
            if name in combinators:
                tags.span('combinator', class_='kind')
                tags += ' '
            if name in basis_functions:
                tags.span('built-in', class_='kind')

        foo(d, '\n'.join(section))

        def_format(d, name)
        add_discussion(d, name)
        add_crosslinks(d, name)
        add_backlinks(d, name)

html_string = '<!DOCTYPE html>' + str(doc)

print(html_string, file=open('../html/FuncRef.html', 'w'))
#from bs4 import BeautifulSoup
#print(BeautifulSoup(html_string, 'html.parser').prettify())

##import pprint
##pprint.pprint(crosslinks)
