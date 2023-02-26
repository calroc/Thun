#include <uvm/syscalls.h>
#include <uvm/utils.h>

#define   RED 0xFF_00_00
#define GREEN 0x00_FF_00

size_t FRAME_WIDTH = 1280;
size_t FRAME_HEIGHT = 800;

// RGBA pixels: 800 * 600
u32 frame_buffer[1024_000];


void anim_callback()
{
	// Clear the screen
	memset(frame_buffer, 0, sizeof(frame_buffer));
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
	window_draw_frame(0, frame_buffer);
	time_delay_cb(100, anim_callback);
}

void main()
{
	window_create(FRAME_WIDTH, FRAME_HEIGHT, "Bouncing Ball Example", 0);
	time_delay_cb(0, anim_callback);
	enable_event_loop();
}
