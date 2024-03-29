#include <stdlib.h>
#include <uvm/syscalls.h>
#include <uvm/utils.h>
#include "/home/sforman/src/Joypy/implementations/uvm-ncc/font/font.h"
#include "/home/sforman/src/Joypy/implementations/uvm-ncc/font/monogram.h"
#include "/home/sforman/src/Joypy/implementations/uvm-ncc/graphics.h"

#define font_data font_PublicPixel_22_data
#define font_width font_PublicPixel_22_width
#define font_height font_PublicPixel_22_height
#define font_numchars font_PublicPixel_22_number_of_characters

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
	if (ch < 0 || dest_x < 0 || dest_y < 0
			|| ch >= font_numchars
			|| dest_x >= (FRAME_WIDTH - font_width)
			|| dest_y >= (FRAME_HEIGHT - font_height))
		// No error message or anything, just decline to draw.
		return;
	carefree_alpha_blend_blit(
		frame_buffer,   // destination
		font_data[ch],  // source
		FRAME_WIDTH,    // destination width (or "stride")
		dest_x,         // top
		dest_y,         // left
		font_width,
		font_height
	);
}


void
draw_monogram_char(u8 ch, u64 dest_x, u64 dest_y, u32 color)
{
	// Check the inputs.
	if (ch < 0 || dest_x < 0 || dest_y < 0
			|| ch > font_monogram_number_of_characters
			|| dest_x >= (FRAME_WIDTH - font_monogram_width)
			|| dest_y >= (FRAME_HEIGHT - font_monogram_height))
		// No error message or anything, just decline to draw.
		return;
	u32* d = frame_buffer + FRAME_WIDTH * dest_y + dest_x;
	for (u64 y = 0; y < font_monogram_height; ++y) {
		u8 pixel_bits = font_monogram_data[ch][y];
		for (u64 x = 0; x < font_monogram_width; ++x) {
			if (!pixel_bits) break;
			if (pixel_bits & 1) {
				*(d+x) = color;
			}
			pixel_bits = pixel_bits >> 1;
		}
		d = d + FRAME_WIDTH;
	}
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
	u8 ch = rand() % font_numchars;
	draw_char(ch, pos_x - font_width, pos_y - font_height);
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
	init_monogram_font_data();
	wid = window_create(FRAME_WIDTH, FRAME_HEIGHT, "Xerblin", 0);
	draw_background(frame_buffer, FRAME_WIDTH, FRAME_HEIGHT);
	for (size_t ch = 0; ch < font_monogram_number_of_characters; ++ch) {
		draw_monogram_char(
			ch,
			129 + (ch % 26) * font_monogram_width,
			129 + (ch / 26) * font_monogram_height,
			0x00000000
		);
	}
	for (size_t ch = 0; ch < font_monogram_number_of_characters; ++ch) {
		draw_monogram_char(
			ch,
			128 + (ch % 26) * font_monogram_width,
			128 + (ch / 26) * font_monogram_height,
			0x00FFFFFF
		);
	}
	u64 w = 3 + 26 * font_monogram_width;
	u64 h = 4 + font_monogram_number_of_characters / 26 * font_monogram_height;
	carefree_draw_box(frame_buffer, FRAME_WIDTH, 126, 126, w, h, WHITE);
	w = 200;
	carefree_wu_line(frame_buffer, FRAME_WIDTH, 0, 0, FRAME_WIDTH, FRAME_HEIGHT-1, WHITE);
	//for (w = 101; w < (FRAME_WIDTH - 10); w = w + 50) {
	//	carefree_wu_line(frame_buffer, FRAME_WIDTH, 10, 256, w, 100, WHITE);
	//}
	window_draw_frame(wid, frame_buffer);
	window_on_keydown(wid, keydown);
	window_on_mousedown(wid, mousedown);
	window_on_mousemove(wid, mousemove);
	enable_event_loop();
}
