from PIL import Image, ImageDraw


def draw_wu_line(draw, x, y, w, h):
    # Without loss of generality only lines in the first oc
    assert(w > 0 and h > 0 and w > h)
    k = 0xffff * h // w
    print(bin(k), hex(k), k)
    x1 = x + w - 1
    y1 = y + h - 1
    print(x1, y1)
    d = 0
    while x1 > x:
        draw.point([(x, y), (x1, y1)], fill=(0, 0, 0, 0xff - (d >> 8)))
        draw.point([(x, y + 1), (x1, y1 - 1)], fill=(0, 0, 0,         d >> 8))
        x += 1
        x1 -= 1
        if d + k >= 0xFFFF:
            d = k - (0xFFFF - d)
            y += 1
            y1 -= 1
        else:
            d += k
    if x1 == x:
        if y1 == y:
            points = [(x, y)]
            alpha = 0xff
        else:
            points = [(x, y), (x, y1)]
            alpha = 0x7f
        draw.point(points, fill=(0, 0, 0, alpha))

size = 100, 50
im = Image.new('RGBA', size)
d = ImageDraw.Draw(im, 'RGBA')

#draw_wu_line(d, 0, 0, *size)

for w in range(51, 100):
    draw_wu_line(d, 0, 0, w, 50)

base = Image.new('RGBA', size, (0xff, 0xff, 0xff, 0xff))
base.alpha_composite(im)
base.save('wu_demo.png')
