import sys
from PIL import Image

txt = (
    'abcdefghijklmnopqrstuvwxyz'
    'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
  '''0123456789@#$&_~|`'"%^=-+*'''
   r'/\<>[]{}(),.;:!?'
    )

font_name, pointsize = sys.argv[-2:] if len(sys.argv) >= 3 else ('Inconsolata', '24')


g = ((i, ch) for i, ch in enumerate(txt))

w, h = Image.open('00.bmp').size

# Check that all the BMPs are the same size.
for i, _ in enumerate(txt):
    with Image.open(f'{i:02}.bmp') as im:
        assert im.size == (w, h), f'bad size {i:02}.bmp {w} x {h}!'

print(f'''\

u64 font_{font_name}_{pointsize}_number_of_characters = {len(txt)};
u64 font_{font_name}_{pointsize}_width = {w};
u64 font_{font_name}_{pointsize}_height = {h};

u32 font_{font_name}_{pointsize}_data[{len(txt)}][{w * h}];


void
init_font_data()
{{
\tmemset(font_{font_name}_{pointsize}_data, 0, {4 * len(txt) * w * h});
''')

for i, ch in enumerate(txt):
    print(f'\t// {repr(ch)}')
    with Image.open(f'{i:02}.bmp') as im:
        data = list(im.getdata())
    for n, (blue, green, red, alpha) in enumerate(data):
        if blue or green or red or alpha:
            print(f'\tfont_{font_name}_{pointsize}_data[{i}][{n}] = 0x{alpha:02x}_{red:02x}_{green:02x}_{blue:02x};')
print(f'}}')

##
##if len(sys.argv) > 1:
##    fn = sys.argv[-1]
##else:
##    fn = "font.bmp"
##
##with Image.open(fn) as im:
##    data = list(im.getdata())
##
##print(f'const int font_width = {im.width};')
##print(f'const int font_height = {im.height};')
##print(f'pixel_t font_data[{len(data)}] = {{')
##print(
##    ',\n'.join(
##        f'\t{{0x{blue:02x}, 0x{green:02x}, 0x{red:02x}, 0x{alpha:02x}}}'
##        for blue, green, red, alpha in data[::1000]
##        )
##    )
##print('};')
##

