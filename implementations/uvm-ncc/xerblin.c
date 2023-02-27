#include <stdlib.h>
#include <uvm/syscalls.h>
#include <uvm/utils.h>
#include "/home/sforman/src/Joypy/implementations/uvm-ncc/font/font.h"
#include "/home/sforman/src/Joypy/implementations/uvm-ncc/graphics.h"

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
	} else if (KEY_TAB == keycode) {
	} else if (KEY_RETURN == keycode) {
	} else if (KEY_SPACE == keycode) {
	} else if (KEY_NUM0 == keycode) {
	} else if (KEY_NUM1 == keycode) {
	} else if (KEY_NUM2 == keycode) {
	} else if (KEY_NUM3 == keycode) {
	} else if (KEY_NUM4 == keycode) {
	} else if (KEY_NUM5 == keycode) {
	} else if (KEY_NUM6 == keycode) {
	} else if (KEY_NUM7 == keycode) {
	} else if (KEY_NUM8 == keycode) {
	} else if (KEY_NUM9 == keycode) {
	} else if (KEY_A == keycode) {
	} else if (KEY_B == keycode) {
	} else if (KEY_C == keycode) {
	} else if (KEY_D == keycode) {
	} else if (KEY_S == keycode) {
	} else if (KEY_W == keycode) {
	} else if (KEY_LEFT == keycode) {
	} else if (KEY_RIGHT == keycode) {
	} else if (KEY_UP == keycode) {
	} else if (KEY_DOWN == keycode) {
	} else if (KEY_SHIFT == keycode) {
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
	u64 w = 3 + 26 * font_Inconsolata_22_width;
	u64 h = 4 + 4 * font_Inconsolata_22_height;
	carefree_draw_box(frame_buffer, FRAME_WIDTH, 126, 126, w, h, WHITE);
	w = 200;
	//carefree_wu_line(frame_buffer, FRAME_WIDTH, 10, 256, 200, 100, WHITE);
	//for (w = 101; w < (FRAME_WIDTH - 10); w = w + 50) {
	//	carefree_wu_line(frame_buffer, FRAME_WIDTH, 10, 256, w, 100, WHITE);
	//}
	window_draw_frame(wid, frame_buffer);
	window_on_keydown(wid, keydown);
	window_on_mousedown(wid, mousedown);
	window_on_mousemove(wid, mousemove);
	enable_event_loop();
}
