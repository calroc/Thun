#include <stddef.h>

#define   RED 0xFF_00_00
#define GREEN 0x00_FF_00
#define WHITE 0xFF_FF_FF

void
draw_background(u32* buffer, size_t w, size_t h)
{
	for (size_t x = 0; x < w; ++x) {
		for (size_t y = 0; y < h; ++y) {
			u8 blue = x & 255;
			u8 green = y & 255;
			u8 red = (x + y + 128) & 255;
			u32* pix_ptr = buffer + w * y + x;
			*pix_ptr = (red << 16) | (green << 8) | blue;
		}
	}
}

void
carefree_alpha_blend_blit(u32* dest, u32* source, size_t dest_stride, u64 dest_x, u64 dest_y, u64 w, u64 h)
{
	u32* d = dest + dest_stride * dest_y + dest_x;
	for (u64 y = 0; y < h; ++y) {
		for (u64 x = 0; x < w; ++x) {
			u32* spix_ptr = source + x + w * y;
			u32 pixel = *spix_ptr;
			u8 alpha = pixel >> 24;
			if (!alpha) continue;
			u32* pix_ptr = d + x + dest_stride * y;
			if (0xFF == alpha) {
				*pix_ptr = pixel;
				continue;
			}
			u32 dest_pixel = *pix_ptr;
			u8 unalpha = 0xFF - alpha;
			u8 red = (((dest_pixel >> 16) & 255) * unalpha + ((pixel >> 16) & 255) * alpha) / 0xff;
			u8 green = (((dest_pixel >> 8) & 255) * unalpha + ((pixel >> 8) & 255) * alpha) / 0xff;
			u8 blue = ((dest_pixel & 255) * unalpha + (pixel & 255) * alpha) / 0xff;
			*pix_ptr = (alpha << 24) | (red << 16) | (green << 8) | blue;
		}
	}
}


void
carefree_draw_horizontal_line(u32* dest, size_t dest_stride, u64 x, u64 y, u64 w, u32 color)
{
	u32* d = dest + dest_stride * y + x;
	for (u64 i = 0; i < w; ++i) {
		*d = color;
		d = d + 1;
	}
}


void
carefree_draw_vertical_line(u32* dest, size_t dest_stride, u64 x, u64 y, u64 h, u32 color)
{
	u32* d = dest + dest_stride * y + x;
	for (u64 i = 0; i < h; ++i) {
		*d = color;
		d = d + dest_stride;
	}
}


void
carefree_draw_box(u32* dest, size_t dest_stride, u64 y, u64 x, u64 w, u64 h, u32 color)
{
	carefree_draw_horizontal_line(dest, dest_stride, x,     y,     w, color);
	carefree_draw_horizontal_line(dest, dest_stride, x,     y + h, w, color);
	carefree_draw_vertical_line(  dest, dest_stride, x,     y,     h, color);
	carefree_draw_vertical_line(  dest, dest_stride, x + w, y,     h, color);
}


