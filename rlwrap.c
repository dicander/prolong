// rlwrap.c — tiny wrappers, no structs in the ABI
#include "raylib.h"
#include <stdlib.h>

void RL_InitWindow(int w, int h, const char *title) { InitWindow(w, h, title); }
void RL_SetTargetFPS(int fps) { SetTargetFPS(fps); }
int  RL_WindowShouldClose(void) { return WindowShouldClose(); }
int  RL_IsKeyDown(int key) { return IsKeyDown(key); }

void RL_BeginDrawing(void) { BeginDrawing(); }
void RL_EndDrawing(void) { EndDrawing(); }
void RL_CloseWindow(void) { CloseWindow(); }

// Elapsed time of the previous frame, in microseconds.
// The game integrates against this instead of assuming a fixed 1/60 s tick.
int RL_GetFrameTimeUs(void) { return (int)(GetFrameTime() * 1e6); }

// Render cadence. WSLg composites on its own ~60 Hz clock and offers no real
// vsync (FLAG_VSYNC_HINT is ignored), so a 60 fps render loop beats against the
// compositor and periodically drops or duplicates frames. Rendering well above
// 60 keeps a fresh frame ready for every composite. PROLONG_FPS overrides
// (0 = uncapped).
void RL_ApplyTargetFPS(void) {
    const char *e = getenv("PROLONG_FPS");
    int fps = 240;
    if (e && *e) {
        char *end;
        long v = strtol(e, &end, 10);
        if (*end == '\0' && v >= 0 && v <= 1000) fps = (int)v;
    }
    if (fps > 0) SetTargetFPS(fps);
}

void RL_ClearBackgroundRGBA(int r,int g,int b,int a) {
    ClearBackground((Color){(unsigned char)r,(unsigned char)g,(unsigned char)b,(unsigned char)a});
}
void RL_DrawRectangleRGBA(int x,int y,int w,int h,int r,int g,int b,int a) {
    DrawRectangle(x,y,w,h,(Color){(unsigned char)r,(unsigned char)g,(unsigned char)b,(unsigned char)a});
}
