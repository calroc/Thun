import sys
import markdown

#filename = './source/Thun.md'
filename = sys.argv[-1]
with open(filename, encoding="UTF_8") as f:
    text = f.read()

def title_of(html):
    return html[html.find('<h1>') + 4:html.find('</h1>')]

#title = title_of(text)

html = markdown.markdown(text, output_format="html5")

print(f'''\
<!doctype html>
<html>
<head>
<meta charset="utf-8">
<title>{title_of(html)}</title>
<link rel="stylesheet" href="/css/fonts.css">
<link rel="stylesheet" href="/css/site.css">
</head>
<body>
{html}
</body>
</html>''')

