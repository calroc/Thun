
void
draw_background(u32* buffer, size_t w, size_t h)
{
	for (size_t x = 0; x < w; ++x) {
		for (size_t y = 0; y < h; ++y) {
			u32* pix_ptr = frame_buffer + w * y + x;
			u8 blue = x & 255;
			u8 green = y & 255;
			u8 red = (x + y + 128) & 255;
			u8 alpha = 123;
			*pix_ptr = (alpha << 24) | (red << 16) | (green << 8) | blue;
		}
	}
}
