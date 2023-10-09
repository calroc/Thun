from html import escape
from re import sub


def j_replace(match):
    name = escape(match.group(0))
    return f'<a href="#{name}">{name}</a>'

def j_bod(body):
    return sub('[^ [\]]+', j_replace, body.strip())

defs = list(open('defs.txt'))

print('<dl>')

for d in defs:
    name, body = d.split(None, 1)
    name = escape(name)
    print(f'	<dt id="{name}">{name}</dt> <dd>{j_bod(body)}</dd><br>')

print('</dl>')
