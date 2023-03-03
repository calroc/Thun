FRAME_WIDTH = 100
color = 1

line_color_buffer = [0] * FRAME_WIDTH

line_color_buffer[0] = color;
offset = 1
while ((offset << 1) < FRAME_WIDTH):
    color += 1
    print(f't: {offset} color: {color}')
    line_color_buffer[offset:offset << 1] = [color] * offset
    offset = offset << 1

print(f't: {offset} color: {color}')

remainder = FRAME_WIDTH - offset
line_color_buffer[offset:] = line_color_buffer[:remainder]
print(line_color_buffer)

##for i, n in enumerate(line_color_buffer):
##    print((i), n)
##    if not n:
##        break

[
    1, 1,
    2, 2,
    3, 3, 3, 3,
    4, 4, 4, 4, 4, 4, 4, 4,
    5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5,
    6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,
    1, 1,
    2, 2,
    3, 3, 3, 3,
    4, 4, 4, 4, 4, 4, 4, 4,
    5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5,
    6, 6, 6, 6
    ]
