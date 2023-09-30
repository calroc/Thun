import hashlib, re
from myhtml import HTML

TITLE = 'Thun Function Reference'

with open('Function-Reference.md') as f:
    md = f.read()

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
    return 'function_' + (
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
        d = b.div
        anchor_id = anchor_for(name)
        title = d.h2(name, id=anchor_id, class_='func_name')
        title += ' '
        title.a('¶', href='#' + anchor_id, class_='self_link')
        if name in combinators:
            d.p.span('combinator', class_='kind')
        d.pre('\n'.join(section))
        

html_string = '<!DOCTYPE html>' + str(doc)

print(html_string, file=open('../html/FuncRef.html', 'w'))
#from bs4 import BeautifulSoup
#print(BeautifulSoup(html_string, 'html.parser').prettify())
