from PIL import Image, ImageDraw


def draw_wu_line(draw, x, y, w, h):
    # Without loss of generality only lines in the first oc
    assert w > 0 and h > 0 and w > h
    k = 0xffff * h // w
    d = k >> 1
    while w > 0:
        w -= 1
        intensity = d >> 8
        draw.point([(x, y    )], fill=(0, 0, 0, 0xff - intensity))
        draw.point([(x, y + 1)], fill=(0, 0, 0,        intensity))
        x += 1
        if d + k >= 0xFFFF:
            d = k - (0xFFFF - d)
            y += 1
        else:
            d += k

size = 100, 50
im = Image.new('RGBA', size)
d = ImageDraw.Draw(im, 'RGBA')

# Diagonal line
#draw_wu_line(d, 0, 0, *size)

# Nearly 45 degrees.
#draw_wu_line(d, 0, 0, 51, 50)

# Nearly horizontal line.
#draw_wu_line(d, 0, 0, 100, 5)

draw_wu_line(d, 0, 0, 100, 33)

base = Image.new('RGBA', size, (0xff, 0xff, 0xff, 0xff))
base.alpha_composite(im)
base.save('wu_demo.png')
