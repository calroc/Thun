#include <uvm/syscalls.h>
#include <uvm/utils.h>

#define   RED 0xFF_00_00
#define GREEN 0x00_FF_00

size_t FRAME_WIDTH = 1280;
size_t FRAME_HEIGHT = 800;

// RGBA pixels: 800 * 600
u32 frame_buffer[1024_000];

int wid;


void
anim_callback()
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
	window_draw_frame(wid, frame_buffer);
	// This rate of refresh of the whole screen caused ~60% CPU use.  :(
	// time_delay_cb(100, anim_callback);
}


//void
//keydown(u64 window_id, u16 keycode)
//{
//	window_draw_frame(window_id, frame_buffer);
//}


void
mousemove(u64 window_id, u64 new_x, u64 new_y)
{
	// Redrawing the window on move seems to repair damage from
	// dragging portion of the window offscreen then back on as
	// you drag, without using too much CPU.
	// No, weird, it doesn't.
	// It did, once, but maybe that was teh X server somehow doing it for me?
	// In any event, moving the mouse over the screen after drag damage
	// works fine, as expected.
	// (I just really would like to know why it stopped damaging the window
	// in the previous try?  Ugh!)
	window_draw_frame(window_id, frame_buffer);
}


void
main()
{
	wid = window_create(FRAME_WIDTH, FRAME_HEIGHT, "Bouncing Ball Example", 0);
	time_delay_cb(0, anim_callback);
	//window_on_keydown(wid, keydown);
	window_on_mousemove(wid, mousemove);
	enable_event_loop();
}
