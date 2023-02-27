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

// Current mouse pointer position
size_t pos_x = 200;
size_t pos_y = 200;


void
draw_char(u8 ch, u64 dest_x, u64 dest_y)
{
	// Check the inputs.
	if (ch < 0 || ch > font_Inconsolata_22_number_of_characters
		|| dest_x < 0 || dest_x >= (FRAME_WIDTH - font_Inconsolata_22_width)
		|| dest_y < 0 || dest_y >= (FRAME_HEIGHT - font_Inconsolata_22_height))
		// No error message or anything, just decline to draw.
		return;
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


void
keydown(u64 window_id, u16 keycode)
{
	if (KEY_BACKSPACE == keycode) {
		exit(0);
	}
	print_i64(keycode);
	print_endl();
}


void
mousedown(u64 window_id, u8 btn_id)
{
	u8 ch = rand() % font_Inconsolata_22_number_of_characters;
	draw_char(ch, pos_x - font_Inconsolata_22_width, pos_y - font_Inconsolata_22_height);
	window_draw_frame(window_id, frame_buffer);
}


void
mousemove(u64 window_id, u64 new_x, u64 new_y)
{
	// Update the mouse position
	pos_x = new_x;
	pos_y = new_y;
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
	window_on_keydown(wid, keydown);
	window_on_mousedown(wid, mousedown);
	window_on_mousemove(wid, mousemove);
	enable_event_loop();
}
