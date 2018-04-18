#define _CRT_SECURE_NO_WARNINGS
#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <stdbool.h>
#include <ctype.h>
#include <assert.h>

#include "noir.c"

#include "SDL.h"

bool check_init(void) {
    if (!noir.init) {
        noir.error = "Not initialized";
        return false;
    }
    return true;
}

int noir_key_to_sdl_scancode[NUM_KEYS] = {
    [KEY_RETURN] = SDL_SCANCODE_RETURN,
    [KEY_SPACE] = SDL_SCANCODE_SPACE,
    [KEY_BACKSPACE] = SDL_SCANCODE_BACKSPACE,
    [KEY_TAB] = SDL_SCANCODE_TAB,
    [KEY_ESCAPE] = SDL_SCANCODE_ESCAPE,
    [KEY_LEFT] = SDL_SCANCODE_LEFT,
    [KEY_RIGHT] = SDL_SCANCODE_RIGHT,
    [KEY_UP] = SDL_SCANCODE_UP,
    [KEY_DOWN] = SDL_SCANCODE_DOWN,
    [KEY_LSHIFT] = SDL_SCANCODE_LSHIFT,
    [KEY_RSHIFT] = SDL_SCANCODE_RSHIFT,
    [KEY_LCTRL] = SDL_SCANCODE_LCTRL,
    [KEY_RCTRL] = SDL_SCANCODE_RCTRL,
    [KEY_LALT] = SDL_SCANCODE_LALT,
    [KEY_RALT] = SDL_SCANCODE_RALT,

};

int sdl_scancode_to_noir_key[SDL_NUM_SCANCODES];

void update_mouse(void) {
    if (noir.mouse.captured != noir.mouse.synced_captured) {
        if (SDL_CaptureMouse(noir.mouse.captured) != 0) {
            noir.error = "Mouse capture failed";
        }
    }
    noir.mouse.synced_captured = noir.mouse.captured;

    if (noir.mouse.pos.x != noir.mouse.synced_pos.x || noir.mouse.pos.y != noir.mouse.synced_pos.y) {
        SDL_WarpMouseInWindow(NULL, noir.mouse.pos.x, noir.mouse.pos.y);
    }
    uint32_t state = SDL_GetMouseState(&noir.mouse.pos.x, &noir.mouse.pos.y);
    noir.mouse.delta_pos = (int2){noir.mouse.pos.x - noir.mouse.synced_pos.x, noir.mouse.pos.y - noir.mouse.synced_pos.y};
    noir.mouse.moved = noir.mouse.delta_pos.x || noir.mouse.delta_pos.y;
    noir.mouse.synced_pos = noir.mouse.pos;

    if (noir.mouse.global_pos.x != noir.mouse.synced_global_pos.x || noir.mouse.global_pos.y != noir.mouse.synced_global_pos.y) {
        SDL_WarpMouseGlobal(noir.mouse.global_pos.x, noir.mouse.global_pos.y);
    }
    SDL_GetGlobalMouseState(&noir.mouse.global_pos.x, &noir.mouse.global_pos.y);
    noir.mouse.global_delta_pos = (int2){noir.mouse.global_pos.x - noir.mouse.synced_global_pos.x, noir.mouse.global_pos.y - noir.mouse.synced_global_pos.y};
    noir.mouse.global_moved = noir.mouse.global_delta_pos.x || noir.mouse.global_delta_pos.y;
    noir.mouse.synced_global_pos = noir.mouse.global_pos;
}

void update_events(void) {
    for (int key = 0; key < NUM_KEYS; key++) {
        reset_digital_button_events(&noir.keys[key]);
    }
    reset_digital_button_events(&noir.mouse.left_button);
    reset_digital_button_events(&noir.mouse.middle_button);
    reset_digital_button_events(&noir.mouse.right_button);
    SDL_Event event;
    char *text_ptr = noir.text;
    char *text_end = noir.text + sizeof(noir.text) - 1;
    while (SDL_PollEvent(&event)) {
        switch (event.type) {
        case SDL_MOUSEBUTTONDOWN:
        case SDL_MOUSEBUTTONUP:
            if (event.button.button == SDL_BUTTON_LEFT) {
                update_digital_button(&noir.mouse.left_button, event.button.state == SDL_PRESSED);
            } else if (event.button.button == SDL_BUTTON_MIDDLE) {
                update_digital_button(&noir.mouse.middle_button, event.button.state == SDL_PRESSED);
            } else if (event.button.button == SDL_BUTTON_RIGHT) {
                update_digital_button(&noir.mouse.right_button, event.button.state == SDL_PRESSED);
            }
            break;
        case SDL_KEYDOWN:
        case SDL_KEYUP:
            if (!event.key.repeat) {
                int key = sdl_scancode_to_noir_key[event.key.keysym.scancode];
                if (key) {
                    update_digital_button(&noir.keys[key], event.key.state == SDL_PRESSED);
                    update_combination_keys();
                }
            }
            break;
        case SDL_TEXTINPUT: {
            const char *str = event.text.text;
            while (*str) {
                if (text_ptr == text_end) {
                    noir.error = "Text buffer overflow";
                    break;
                }
                *text_ptr++ = *str++;
            }
            break;
        }
        case SDL_QUIT:
            noir.quit = true;
            break;
        }
    }
    *text_ptr = 0;
}

void update_time(void) {
    uint64 ticks = SDL_GetPerformanceCounter() - noir.time.sdl_start_ticks;
    noir.time.ticks = ticks;

    noir.time.nsecs = (noir.time.ticks * 1000 * 1000 * 1000) / noir.time.ticks_per_sec;
    noir.time.usecs = (noir.time.ticks * 1000 * 1000) / noir.time.ticks_per_sec;
    noir.time.msecs = (noir.time.ticks * 1000) / noir.time.ticks_per_sec;
    noir.time.secs = (double)noir.time.ticks / (double)noir.time.ticks_per_sec;

    noir.time.delta_ticks = (int)(ticks - noir.time.ticks);
    noir.time.delta_nsecs = (int)((noir.time.delta_ticks * 1000 * 1000 * 1000) / noir.time.ticks_per_sec);
    noir.time.delta_usecs = (int)((noir.time.delta_ticks * 1000 * 1000) / noir.time.ticks_per_sec);
    noir.time.delta_msecs = (int)((noir.time.delta_ticks * 1000) / noir.time.ticks_per_sec);
    noir.time.delta_secs = (float)noir.time.delta_ticks / (float)noir.time.ticks_per_sec;
}

void update_window(void) {
    if (noir.window.pos.x != noir.window.synced_pos.x || noir.window.pos.y != noir.window.synced_pos.y) {
        SDL_SetWindowPosition(noir.window.sdl_window, noir.window.pos.x, noir.window.pos.y);
    }
    int x, y;
    SDL_GetWindowPosition(noir.window.sdl_window, &x, &y);
    noir.window.moved = noir.num_updates == 0 || noir.window.synced_pos.x != x || noir.window.synced_pos.y != y;
    noir.window.pos.x = x;
    noir.window.pos.y = y;
    noir.window.synced_pos = noir.window.pos;

    if (noir.window.size.x != noir.window.synced_size.x || noir.window.size.y != noir.window.synced_size.y) {
        SDL_SetWindowSize(noir.window.sdl_window, noir.window.size.x, noir.window.size.y);
    }
    int width, height;
    SDL_GetWindowSize(noir.window.sdl_window, &width, &height);
    noir.window.resized = noir.num_updates == 0 || noir.window.synced_size.x != width || noir.window.synced_size.y != height;
    noir.window.size.x = width;
    noir.window.size.y = height;
    noir.window.synced_size = noir.window.size;

    if (noir.window.resizable != noir.window.synced_resizable) {
        SDL_SetWindowResizable(noir.window.sdl_window, noir.window.resizable);
    }
    noir.window.synced_resizable = noir.window.resizable;

    if (noir.window.hidden != noir.window.synced_hidden) {
        if (noir.window.hidden) {
            SDL_HideWindow(noir.window.sdl_window);
        } else {
            SDL_ShowWindow(noir.window.sdl_window);
        }
    }
    noir.window.synced_hidden = noir.window.hidden;
}

bool update(void) {
    if (!check_init()) {
        return false;
    }
    SDL_PumpEvents();
    update_events();
    update_window();
    update_time();
    update_mouse();
    noir.num_updates++;
    return !noir.quit;
}

void init_keys(void) {
    for (int c = 0; c < 256; c++) {
        if (isprint(c)) {
            char str[] = {c, 0};
            SDL_Scancode scancode = SDL_GetScancodeFromName(str);
            if (scancode != SDL_SCANCODE_UNKNOWN) {
                noir_key_to_sdl_scancode[(unsigned char)c] = scancode;
            }
        }
    }
    for (int key = 0; key < NUM_KEYS; key++) {
        int scancode = noir_key_to_sdl_scancode[key];
        if (scancode) {
            sdl_scancode_to_noir_key[scancode] = key;
        }
    }
}

bool init_window(void) {
    if (!noir.window.title) {
        noir.window.title = default_window_title;
    }
    int x = noir.window.pos.x == DEFAULT_WINDOW_POS  ? SDL_WINDOWPOS_CENTERED : noir.window.pos.x;
    int y = noir.window.pos.y == DEFAULT_WINDOW_POS  ? SDL_WINDOWPOS_CENTERED : noir.window.pos.y;
    int width = noir.window.size.x == 0 ? default_window_size.x : noir.window.size.x;
    int height = noir.window.size.y == 0 ? default_window_size.y : noir.window.size.y;
    SDL_WindowFlags flags = 0;
    if (noir.window.resizable) {
        flags |= SDL_WINDOW_RESIZABLE;
    }
    if (noir.window.hidden) {
        flags |= SDL_WINDOW_HIDDEN;
    }
    SDL_Window *sdl_window = SDL_CreateWindow(noir.window.title, x, y, width, height, flags);
    if (!sdl_window) {
        noir.error = "Window creation failed";
        return false;
    }
    noir.window.sdl_window = sdl_window;
    noir.window.synced_pos = noir.window.pos;
    update_window();
    return true;
}

void init_time(void) {
    noir.time.ticks_per_sec = SDL_GetPerformanceFrequency();
    noir.time.sdl_start_ticks = SDL_GetPerformanceCounter();
}

bool init(void) {
    if (noir.init) {
        return true;
    }
    if (SDL_Init(SDL_INIT_EVERYTHING) != 0) {
        noir.error = "Initialization failed";
        return false;
    }
    if (!init_window()) {
        return false;
    }
    init_keys();
    init_time();
    noir.init = true;
    return true;
}
