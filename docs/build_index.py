import sys
import markdown

#filename = '/usr/home/sforman/src/Joypy/README.md'
filename = sys.argv[-1]
with open(filename) as f:
    text = f.read()

html = markdown.markdown(text, output_format="html5")

print(f'''\
<!doctype html>
<html>
<head>
<meta charset="utf-8">
<title>Thun</title>
<link rel="stylesheet" href="/css/fonts.css">
<link rel="stylesheet" href="/css/site.css">
</head>
<body>
{html}
</body>
</html>
''')


# <meta http-equiv="Content-Security-Policy" content="default-src 'self'" />
# <link rel="stylesheet" href="/css/fonts.css">

