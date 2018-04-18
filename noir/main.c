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
    if (!app.init) {
        app.error = "Not initialized";
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
    if (app.mouse.capture != app.mouse.synced_capture) {
        if (SDL_CaptureMouse(app.mouse.capture) != 0) {
            app.error = "Mouse capture failed";
        }
    }
    app.mouse.synced_capture = app.mouse.capture;

    if (app.mouse.pos.x != app.mouse.synced_pos.x || app.mouse.pos.y != app.mouse.synced_pos.y) {
        SDL_WarpMouseInWindow(NULL, app.mouse.pos.x, app.mouse.pos.y);
    }
    uint32_t state = SDL_GetMouseState(&app.mouse.pos.x, &app.mouse.pos.y);
    app.mouse.delta_pos = (int2){app.mouse.pos.x - app.mouse.synced_pos.x, app.mouse.pos.y - app.mouse.synced_pos.y};
    app.mouse.moved = app.mouse.delta_pos.x || app.mouse.delta_pos.y;
    app.mouse.synced_pos = app.mouse.pos;

    if (app.mouse.global_pos.x != app.mouse.synced_global_pos.x || app.mouse.global_pos.y != app.mouse.synced_global_pos.y) {
        SDL_WarpMouseGlobal(app.mouse.global_pos.x, app.mouse.global_pos.y);
    }
    SDL_GetGlobalMouseState(&app.mouse.global_pos.x, &app.mouse.global_pos.y);
    app.mouse.global_delta_pos = (int2){app.mouse.global_pos.x - app.mouse.synced_global_pos.x, app.mouse.global_pos.y - app.mouse.synced_global_pos.y};
    app.mouse.global_moved = app.mouse.global_delta_pos.x || app.mouse.global_delta_pos.y;
    app.mouse.synced_global_pos = app.mouse.global_pos;
}

void update_events(void) {
    for (int key = 0; key < NUM_KEYS; key++) {
        reset_digital_button_events(&app.keys[key]);
    }
    reset_digital_button_events(&app.mouse.left_button);
    reset_digital_button_events(&app.mouse.middle_button);
    reset_digital_button_events(&app.mouse.right_button);
    SDL_Event event;
    char *text_ptr = app.text;
    char *text_end = app.text + sizeof(app.text) - 1;
    while (SDL_PollEvent(&event)) {
        switch (event.type) {
        case SDL_MOUSEBUTTONDOWN:
        case SDL_MOUSEBUTTONUP:
            if (event.button.button == SDL_BUTTON_LEFT) {
                update_digital_button(&app.mouse.left_button, event.button.state == SDL_PRESSED);
            } else if (event.button.button == SDL_BUTTON_MIDDLE) {
                update_digital_button(&app.mouse.middle_button, event.button.state == SDL_PRESSED);
            } else if (event.button.button == SDL_BUTTON_RIGHT) {
                update_digital_button(&app.mouse.right_button, event.button.state == SDL_PRESSED);
            }
            break;
        case SDL_KEYDOWN:
        case SDL_KEYUP:
            if (!event.key.repeat) {
                int key = sdl_scancode_to_noir_key[event.key.keysym.scancode];
                if (key) {
                    update_digital_button(&app.keys[key], event.key.state == SDL_PRESSED);
                    update_combination_keys();
                }
            }
            break;
        case SDL_TEXTINPUT: {
            const char *str = event.text.text;
            while (*str) {
                if (text_ptr == text_end) {
                    app.error = "Text buffer overflow";
                    break;
                }
                *text_ptr++ = *str++;
            }
            break;
        }
        case SDL_QUIT:
            app.quit = true;
            break;
        }
    }
    *text_ptr = 0;
}

void update_time(void) {
    uint64 ticks = SDL_GetPerformanceCounter() - app.time.sdl_start_ticks;
    app.time.ticks = ticks;

    app.time.nsecs = (app.time.ticks * 1000 * 1000 * 1000) / app.time.ticks_per_sec;
    app.time.usecs = (app.time.ticks * 1000 * 1000) / app.time.ticks_per_sec;
    app.time.msecs = (app.time.ticks * 1000) / app.time.ticks_per_sec;
    app.time.secs = (double)app.time.ticks / (double)app.time.ticks_per_sec;

    app.time.delta_ticks = (int)(ticks - app.time.ticks);
    app.time.delta_nsecs = (int)((app.time.delta_ticks * 1000 * 1000 * 1000) / app.time.ticks_per_sec);
    app.time.delta_usecs = (int)((app.time.delta_ticks * 1000 * 1000) / app.time.ticks_per_sec);
    app.time.delta_msecs = (int)((app.time.delta_ticks * 1000) / app.time.ticks_per_sec);
    app.time.delta_secs = (float)app.time.delta_ticks / (float)app.time.ticks_per_sec;
}

void update_window(void) {
    if (app.window.title != app.window.synced_title) {
        SDL_SetWindowTitle(app.window.sdl_window, app.window.title);
        strcpy_s(app.window.synced_title, sizeof(app.window.synced_title), app.window.title);
        app.window.title = app.window.synced_title;
    }

    if (app.window.pos.x != app.window.synced_pos.x || app.window.pos.y != app.window.synced_pos.y) {
        SDL_SetWindowPosition(app.window.sdl_window, app.window.pos.x, app.window.pos.y);
    }
    int x, y;
    SDL_GetWindowPosition(app.window.sdl_window, &x, &y);
    app.window.moved = app.num_updates == 0 || app.window.synced_pos.x != x || app.window.synced_pos.y != y;
    app.window.pos.x = x;
    app.window.pos.y = y;
    app.window.synced_pos = app.window.pos;

    if (app.window.size.x != app.window.synced_size.x || app.window.size.y != app.window.synced_size.y) {
        SDL_SetWindowSize(app.window.sdl_window, app.window.size.x, app.window.size.y);
    }
    int width, height;
    SDL_GetWindowSize(app.window.sdl_window, &width, &height);
    app.window.resized = app.num_updates == 0 || app.window.synced_size.x != width || app.window.synced_size.y != height;
    app.window.size.x = width;
    app.window.size.y = height;
    app.window.synced_size = app.window.size;

    if (app.window.resizable != app.window.synced_resizable) {
        SDL_SetWindowResizable(app.window.sdl_window, app.window.resizable);
    }
    app.window.synced_resizable = app.window.resizable;

    if (app.window.hidden != app.window.synced_hidden) {
        if (app.window.hidden) {
            SDL_HideWindow(app.window.sdl_window);
        } else {
            SDL_ShowWindow(app.window.sdl_window);
        }
    }
    app.window.synced_hidden = app.window.hidden;
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
    app.num_updates++;
    return !app.quit;
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
    if (!app.window.title) {
        app.window.title = default_window_title;
    }
    int x = app.window.pos.x == DEFAULT_WINDOW_POS  ? SDL_WINDOWPOS_CENTERED : app.window.pos.x;
    int y = app.window.pos.y == DEFAULT_WINDOW_POS  ? SDL_WINDOWPOS_CENTERED : app.window.pos.y;
    int width = app.window.size.x == 0 ? default_window_size.x : app.window.size.x;
    int height = app.window.size.y == 0 ? default_window_size.y : app.window.size.y;
    SDL_WindowFlags flags = 0;
    if (app.window.resizable) {
        flags |= SDL_WINDOW_RESIZABLE;
    }
    if (app.window.hidden) {
        flags |= SDL_WINDOW_HIDDEN;
    }
    SDL_Window *sdl_window = SDL_CreateWindow(app.window.title, x, y, width, height, flags);
    if (!sdl_window) {
        app.error = "Window creation failed";
        return false;
    }
    app.window.sdl_window = sdl_window;
    app.window.synced_pos = app.window.pos;
    strcpy_s(app.window.synced_title, sizeof(app.window.synced_title), app.window.title);
    update_window();
    return true;
}

void init_time(void) {
    app.time.ticks_per_sec = SDL_GetPerformanceFrequency();
    app.time.sdl_start_ticks = SDL_GetPerformanceCounter();
}

bool init(void) {
    if (app.init) {
        return true;
    }
    if (SDL_Init(SDL_INIT_EVERYTHING) != 0) {
        app.error = "Initialization failed";
        return false;
    }
    if (!init_window()) {
        return false;
    }
    init_keys();
    init_time();
    app.init = true;
    return true;
}
