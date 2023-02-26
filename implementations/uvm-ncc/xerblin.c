#include <stdlib.h>
#include <uvm/syscalls.h>
#include <uvm/utils.h>
#include "/home/sforman/src/Joypy/implementations/uvm-ncc/font/font.h"
#include "/home/sforman/src/Joypy/implementations/uvm-ncc/graphics.h"

#define   RED 0xFF_00_00
#define GREEN 0x00_FF_00

size_t FRAME_WIDTH = 768;
size_t FRAME_HEIGHT = 512;
u32 frame_buffer[393216];

int wid;  // Window ID.


void
draw_char(u8 ch, u64 dest_x, u64 dest_y)
{
	// Check the inputs.
	// Trust that the type "u-" means we don't have to check lower bounds?
	if (ch > font_Inconsolata_22_number_of_characters) {
		// No error message or anything, just decline to draw.
		return;
	}
	if (dest_x >= (FRAME_WIDTH - font_Inconsolata_22_width)) {
		return;
	}
	if (dest_y >= (FRAME_HEIGHT - font_Inconsolata_22_height)) {
		return;
	}
	carefree_alpha_blend_blit(
		frame_buffer,
		font_Inconsolata_22_data[ch],
		FRAME_WIDTH,
		dest_x,
		dest_y,
		font_Inconsolata_22_width,
		font_Inconsolata_22_height
	);
}


//void
//keydown(u64 window_id, u16 keycode)
//{
//	window_draw_frame(window_id, frame_buffer);
//}


void
mousedown(u64 window_id, u64 new_x, u64 new_y)
{
	u32* pix_ptr = frame_buffer + FRAME_WIDTH * new_y + new_x;
	*pix_ptr = GREEN;
	window_draw_frame(window_id, frame_buffer);
}


void
main()
{
	init_font_data();
	wid = window_create(FRAME_WIDTH, FRAME_HEIGHT, "Xerblin", 0);
	draw_background(frame_buffer, FRAME_WIDTH, FRAME_HEIGHT);
	for (size_t ch = 0; ch < font_Inconsolata_22_number_of_characters; ++ch) {
		draw_char(
			ch,
			128 + (ch % 26) * font_Inconsolata_22_width,
			128 + (ch / 26) * font_Inconsolata_22_height
		);
	}
	window_draw_frame(wid, frame_buffer);
	//window_on_keydown(wid, keydown);
	window_on_mousedown(wid, mousedown);
	enable_event_loop();
}
