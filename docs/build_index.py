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

rel = '../' if 'notebooks' in filename else ''

print(f'''\
<!doctype html>
<html>
<head>
<meta charset="utf-8">
<title>{title_of(html)}</title>
<link rel="stylesheet" href="{rel}css/font/fonts.css">
<link rel="stylesheet" href="{rel}css/site.css">
<script src="{rel}Joy.js"></script>
</head>
<body>
<div id="joy_interpreter"></div>
{html}
<script>var joy_interpreter = Elm.Main.init({{node: document.getElementById('joy_interpreter')}});</script>
</body>
</html>''')

