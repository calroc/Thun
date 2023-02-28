#include <assert.h>
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
draw_cicada_bars_background(u32* buffer, size_t w, size_t h)
{
	for (size_t x = 0; x < w; ++x) {
		for (size_t y = 0; y < h; ++y) {
			u8 blue  = 5*x%17 + 127;
			u8 green = 5*x%37 + 127;
			u8 red   = 5*x%43 + 127;
			u32* pix_ptr = buffer + w * y + x;
			*pix_ptr = (red << 16) | (green << 8) | blue;
		}
	}
}


void
draw_cicada_bars_background0(u32* buffer, size_t w, size_t h)
{
	for (size_t x = 0; x < w; ++x) {
		for (size_t y = 0; y < h; ++y) {
			u8 blue  = !((5*x / 17) & 1) ? 100 : 0;
			u8 green = !((5*x / 37) & 1) ? 100 : 0;
			u8 red   = !((5*x / 43) & 1) ? 100 : 0;
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


void
carefree_alpha_blend_plot_pixel(u32* dest, size_t dest_stride, u64 x, u64 y, u32 color, u8 alpha)
{
	if (!alpha) return;
	u32* pix_ptr = dest + dest_stride * y + x;
	if (0xFF == alpha) {
		*pix_ptr = color;
		return;
	}
	u32 dest_pixel = *pix_ptr;
	u8 unalpha = 0xFF - alpha;
	u8 red = (((dest_pixel >> 16) & 255) * unalpha + ((color >> 16) & 255) * alpha) / 0xff;
	u8 green = (((dest_pixel >> 8) & 255) * unalpha + ((color >> 8) & 255) * alpha) / 0xff;
	u8 blue = ((dest_pixel & 255) * unalpha + (color & 255) * alpha) / 0xff;
	*pix_ptr = (red << 16) | (green << 8) | blue;
}


void
carefree_wu_line(u32* dest, size_t dest_stride, u64 x, u64 y, u64 w, u64 h, u32 color)
{
	// This isn't quite Wu's algorithm, although it uses the same
	// fundamental trick of keeping track of both the intensity of
	// the pixels to draw and the right time to increment the minor
	// axis in a single error term.
	//
	// "An Efficient Antialiasing Technique", Xiaolin Wu
	// Computer Graphics, Volume 25, Number 4, July 1991
	// https://dl.acm.org/doi/pdf/10.1145/127719.122734
	//
	// "Graphics Programming Black Book" by Michael Abrash, chapter 42
	// https://archive.org/details/gpbb20

	// > Without loss of generality only lines in the first octant are considered.
	assert(w > 0 && h > 0 && w > h);

	// > We translate the point (x0, y0) to the origin,
	// so y = kx where k = h/w with k <= 1
	u16 k = 0xFFFF * h / w;
	u16 d = k >> 1;
	while (w) {
		w = w - 1;
		u8 intensity = d >> 8;
		carefree_alpha_blend_plot_pixel(dest, dest_stride, x, y + 1, color,  intensity);
		carefree_alpha_blend_plot_pixel(dest, dest_stride, x, y,     color, 0xFF - intensity);
		++x;
		if (d + k >= 0xFFFF) {
			d = k - (0xFFFF - d);
			++y;
		} else {
			d = d + k;
		}
	}
}

