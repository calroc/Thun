#include <uvm/syscalls.h>
#include <uvm/utils.h>
#include "/home/sforman/src/Joypy/implementations/uvm-ncc/font/font.h"

#define   RED 0xFF_00_00
#define GREEN 0x00_FF_00

size_t FRAME_WIDTH = 768;
size_t FRAME_HEIGHT = 512;
u32 frame_buffer[393216];

int wid;


void
draw_char(u8 ch, u64 dest_x, u64 dest_y)
{
	u32* dest = frame_buffer + FRAME_WIDTH * dest_y + dest_x;
	u32* character_data = font_Inconsolata_22_data[ch];
	for (size_t x = 0; x < font_Inconsolata_22_width; ++x) {
		for (size_t y = 0; y < font_Inconsolata_22_height; ++y) {
			u32* pix_ptr = dest + x + FRAME_WIDTH * y;
			u32 pixel = character_data[x + font_Inconsolata_22_width * y];
			u8 alpha = pixel >> 24;
			if (!alpha) {  // no alpha
				continue;
			}
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
draw_background()
{
	for (size_t x = 0; x < FRAME_WIDTH; ++x) {
		for (size_t y = 0; y < FRAME_HEIGHT; ++y) {
			u32* pix_ptr = frame_buffer + FRAME_WIDTH * y + x;
			u8 blue = x & 255;
			u8 green = y & 255;
			u8 red = (x + y + 128) & 255;
			u8 alpha = 123;
			*pix_ptr = (alpha << 24) | (red << 16) | (green << 8) | blue;
		}
	}
}


//void
//keydown(u64 window_id, u16 keycode)
//{
//	window_draw_frame(window_id, frame_buffer);
//}


void
mousemove(u64 window_id, u64 new_x, u64 new_y)
{
	u32* pix_ptr = frame_buffer + FRAME_WIDTH * new_y + new_x;
	*pix_ptr = GREEN;
	window_draw_frame(window_id, frame_buffer);
}


void
main()
{
	init_font_data();
	wid = window_create(FRAME_WIDTH, FRAME_HEIGHT, "Bouncing Ball Example", 0);
	draw_background();
	for (size_t ch = 0; ch < 94; ++ch) {
		draw_char(
			ch,
			128 + (ch % 26) * font_Inconsolata_22_width,
			128 + (ch / 26) * font_Inconsolata_22_height
		);
	}
	window_draw_frame(wid, frame_buffer);
	//window_on_keydown(wid, keydown);
	window_on_mousemove(wid, mousemove);
	enable_event_loop();
}
