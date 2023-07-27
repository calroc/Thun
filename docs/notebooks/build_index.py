import pathlib, re, sys
import markdown


url_base = pathlib.Path('/notebooks')

r = re.compile('<title>(.*)</title>')

def title_of(html_path):
    return r.search(html_path.read_text()).group(1).replace('_', ' ')
    # Just explode if there's no match.
    # NoneType has no "group" attr...


notebook_dir = pathlib.Path('.')
htmls = sorted(
    (
        path
        for path in notebook_dir.glob('*.html')
        if not path.name == 'index.html'
        ),
    key=title_of,
    )

text = ['''\
[Thun](/)

# Notebooks


Various Jupyter notebooks, some more polished than others, a few incomplete.
I'm in the process of rewriting them to use the Joy kernel.

''']

for p in htmls:
    text.append(f'* [{title_of(p)}]({(url_base/p.name)!s})\n')

html = markdown.markdown(''.join(text), output_format="html5")

print(f'''\
<!doctype html>
<html>
<head>
<meta charset="utf-8">
<title>Thun Notebooks</title>
<link rel="stylesheet" href="/css/fonts.css">
<link rel="stylesheet" href="/css/site.css">
</head>
<body>
{html}
</body>
</html>
''')
