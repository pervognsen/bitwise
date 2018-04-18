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

int noir_to_sdl_scancode[NUM_KEYS] = {
    [KEY_RETURN] = SDL_SCANCODE_RETURN,
    [KEY_SPACE] = SDL_SCANCODE_SPACE,
    [KEY_BACKSPACE] = SDL_SCANCODE_BACKSPACE,
    [KEY_TAB] = SDL_SCANCODE_TAB,
    [KEY_ESCAPE] = SDL_SCANCODE_ESCAPE,
    [KEY_LEFT] = SDL_SCANCODE_LEFT,
    [KEY_RIGHT] = SDL_SCANCODE_RIGHT,
    [KEY_UP] = SDL_SCANCODE_UP,
    [KEY_DOWN] = SDL_SCANCODE_DOWN,
};

void update_keys() {
    const uint8_t *sdl_keys = SDL_GetKeyboardState(NULL);
    for (int key = 0; key < NUM_KEYS; key++) {
        int scancode = noir_to_sdl_scancode[key];
        if (scancode) {
            update_digital_button(&noir.keys[key], sdl_keys[scancode]);
        }
    }
    struct {
        int key;
        uint32_t mask;
    } sdl_mod_map[] = {
        {KEY_LSHIFT, KMOD_LSHIFT},
        {KEY_RSHIFT, KMOD_RSHIFT},
        {KEY_SHIFT, KMOD_SHIFT},
        {KEY_LCTRL, KMOD_LCTRL},
        {KEY_RCTRL, KMOD_RCTRL},
        {KEY_CTRL, KMOD_CTRL},
        {KEY_LALT, KMOD_LALT},
        {KEY_RALT, KMOD_RALT},
        {KEY_ALT, KMOD_ALT},
        {KEY_CAPS, KMOD_CAPS},
    };
    SDL_Keymod sdl_mod = SDL_GetModState();
    for (int i = 0; i < sizeof(sdl_mod_map)/sizeof(*sdl_mod_map); i++) {
        uint32_t mask = sdl_mod_map[i].mask;
        int key = sdl_mod_map[i].key;
        update_digital_button(&noir.keys[key], sdl_mod & mask);
    }
}

void update_mouse(void) {
    if (noir.mouse.captured != noir.mouse.synced_captured) {
        if (SDL_CaptureMouse(noir.mouse.captured) != 0) {
            noir.error = "Mouse capture failed";
        }
    }
    noir.mouse.synced_captured = noir.mouse.captured;

    int x, y;
    uint32_t state = SDL_GetMouseState(&x, &y);
    update_digital_button(&noir.mouse.left_button, state & SDL_BUTTON_LEFT);
    update_digital_button(&noir.mouse.middle_button, state & SDL_BUTTON_MIDDLE);
    update_digital_button(&noir.mouse.right_button, state & SDL_BUTTON_RIGHT);
    int2 new_pos;
    if (noir.mouse.pos.x != noir.mouse.synced_pos.x || noir.mouse.pos.y != noir.mouse.synced_pos.y) {
        SDL_WarpMouseInWindow(NULL, noir.mouse.pos.x, noir.mouse.pos.y);
        new_pos = noir.mouse.pos;
    } else {
        new_pos = (int2){x, y};
    }
    noir.mouse.delta_pos = (int2){new_pos.x - noir.mouse.pos.x, new_pos.y - noir.mouse.pos.y};
    noir.mouse.moved = noir.mouse.delta_pos.x || noir.mouse.delta_pos.y;
    noir.mouse.pos = new_pos;
    noir.mouse.synced_pos = new_pos;

    SDL_GetGlobalMouseState(&x, &y);
    if (noir.mouse.global_pos.x != noir.mouse.synced_global_pos.x || noir.mouse.global_pos.y != noir.mouse.synced_global_pos.y) {
        SDL_WarpMouseGlobal(noir.mouse.global_pos.x, noir.mouse.global_pos.y);
    } else {
        noir.mouse.global_pos = (int2){x, y};
    }
    noir.mouse.synced_global_pos = noir.mouse.global_pos;
}

void update_events(void) {
    SDL_Event event;
    char *text_ptr = noir.text;
    char *text_end = noir.text + sizeof(noir.text) - 1;
    while (SDL_PollEvent(&event)) {
        switch (event.type) {
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
    noir.time.delta_ticks = ticks - noir.time.ticks;
    noir.time.ticks = ticks;

    noir.time.nsecs = (noir.time.ticks * 1000 * 1000 * 1000) / noir.time.ticks_per_sec;
    noir.time.usecs = (noir.time.ticks * 1000 * 1000) / noir.time.ticks_per_sec;
    noir.time.msecs = (noir.time.ticks * 1000) / noir.time.ticks_per_sec;
    noir.time.secs = (double)noir.time.ticks / (double)noir.time.ticks_per_sec;

    noir.time.delta_nsecs = (noir.time.delta_ticks * 1000 * 1000 * 1000) / noir.time.ticks_per_sec;
    noir.time.delta_usecs = (noir.time.delta_ticks * 1000 * 1000) / noir.time.ticks_per_sec;
    noir.time.delta_msecs = (noir.time.delta_ticks * 1000) / noir.time.ticks_per_sec;
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
    update_keys();
    update_mouse();
    noir.num_updates++;
    return !noir.quit;
}

void init_scancodes(void) {
    for (int c = 0; c < 256; c++) {
        if (isprint(c)) {
            char str[] = {c, 0};
            SDL_Scancode scancode = SDL_GetScancodeFromName(str);
            if (scancode != SDL_SCANCODE_UNKNOWN) {
                noir_to_sdl_scancode[(unsigned char)c] = scancode;
            }
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
    init_scancodes();
    init_time();
    noir.init = true;
    return true;
}
