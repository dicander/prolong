// rlwrap.c — tiny wrappers, no structs in the ABI
#include "raylib.h"

void RL_InitWindow(int w, int h, const char *title) { InitWindow(w, h, title); }
void RL_SetTargetFPS(int fps) { SetTargetFPS(fps); }
int  RL_WindowShouldClose(void) { return WindowShouldClose(); }
int  RL_IsKeyDown(int key) { return IsKeyDown(key); }

void RL_BeginDrawing(void) { BeginDrawing(); }
void RL_EndDrawing(void) { EndDrawing(); }
void RL_CloseWindow(void) { CloseWindow(); }

void RL_ClearBackgroundRGBA(int r,int g,int b,int a) {
    ClearBackground((Color){(unsigned char)r,(unsigned char)g,(unsigned char)b,(unsigned char)a});
}
void RL_DrawRectangleRGBA(int x,int y,int w,int h,int r,int g,int b,int a) {
    DrawRectangle(x,y,w,h,(Color){(unsigned char)r,(unsigned char)g,(unsigned char)b,(unsigned char)a});
}
