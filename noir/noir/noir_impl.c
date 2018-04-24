#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <stdbool.h>
#include <ctype.h>
#include <assert.h>

#include "SDL.h"

static int noir_key_to_sdl_scancode[noir_NUM_KEYS] = {
    [noir_KEY_RETURN] = SDL_SCANCODE_RETURN,
    [noir_KEY_SPACE] = SDL_SCANCODE_SPACE,
    [noir_KEY_BACKSPACE] = SDL_SCANCODE_BACKSPACE,
    [noir_KEY_TAB] = SDL_SCANCODE_TAB,
    [noir_KEY_ESCAPE] = SDL_SCANCODE_ESCAPE,
    [noir_KEY_LEFT] = SDL_SCANCODE_LEFT,
    [noir_KEY_RIGHT] = SDL_SCANCODE_RIGHT,
    [noir_KEY_UP] = SDL_SCANCODE_UP,
    [noir_KEY_DOWN] = SDL_SCANCODE_DOWN,
    [noir_KEY_LSHIFT] = SDL_SCANCODE_LSHIFT,
    [noir_KEY_RSHIFT] = SDL_SCANCODE_RSHIFT,
    [noir_KEY_LCTRL] = SDL_SCANCODE_LCTRL,
    [noir_KEY_RCTRL] = SDL_SCANCODE_RCTRL,
    [noir_KEY_LALT] = SDL_SCANCODE_LALT,
    [noir_KEY_RALT] = SDL_SCANCODE_RALT,
};

static int sdl_scancode_to_noir_key[SDL_NUM_SCANCODES];

static void sdl_error(const char *name) {
    const char *error = SDL_GetError();
    if (*error) {
        snprintf(noir_app.error_buf, sizeof(noir_app.error_buf), "%s: %s", name, error);
        noir_app.error = noir_app.error_buf;
    }
}

static void sdl_audio_callback(void *userdata, uint8 *buf, int len) {
    if (noir_app.audio.callback) {
        noir_app.audio.callback(noir_app.audio.context, (noir_float2 *)buf, len / sizeof(noir_float2));
    }
}

static bool init_audio(void) {
    noir_app.audio.rate = 44100;
    noir_app.audio.channels = 2;
    SDL_AudioSpec desired_spec = {
        .freq = noir_app.audio.rate,
        .channels = noir_app.audio.channels,
        .format = AUDIO_F32,
        .samples = 4096,
        .callback = sdl_audio_callback,
    };
    SDL_AudioSpec obtained_spec;
    int sdl_device = SDL_OpenAudioDevice(NULL, 0, &desired_spec, &obtained_spec, 0);
    if (sdl_device < 0) {
        sdl_error("SDL_OpenAudioDevice");
        return false;
    }
    noir_app.audio.valid = true;
    noir_app.audio.sdl_device = sdl_device;
    return true;
}

static bool init_display(void) {
    float dpi;
    if (SDL_GetDisplayDPI(0, &dpi, NULL, NULL) != 0) {
        sdl_error("SDL_GetDisplayDPI");
        return false;
    }
    noir_app.display.dpi = dpi;

    SDL_DisplayMode mode;
    if (SDL_GetCurrentDisplayMode(0, &mode) != 0) {
        sdl_error("SDL_GetCurrentDisplayMode");
        return false;
    }
    noir_app.display.size.x = mode.w;
    noir_app.display.size.y = mode.h;
    noir_app.display.rate = mode.refresh_rate;
    return true;
}

static void init_keys(void) {
    for (int c = 0; c < 256; c++) {
        if (isprint(c)) {
            char str[] = {c, 0};
            SDL_Scancode scancode = SDL_GetScancodeFromName(str);
            if (scancode != SDL_SCANCODE_UNKNOWN) {
                noir_key_to_sdl_scancode[(unsigned char)c] = scancode;
            }
        }
    }
    for (int key = 0; key < noir_NUM_KEYS; key++) {
        int scancode = noir_key_to_sdl_scancode[key];
        if (scancode) {
            sdl_scancode_to_noir_key[scancode] = key;
        }
    }
}

static void update_window();

static bool init_window(void) {
    if (!noir_app.window.title) {
        noir_app.window.title = noir_default_window_title;
    }
    int x = noir_app.window.pos.x == noir_DEFAULT_WINDOW_POS  ? SDL_WINDOWPOS_CENTERED : noir_app.window.pos.x;
    int y = noir_app.window.pos.y == noir_DEFAULT_WINDOW_POS  ? SDL_WINDOWPOS_CENTERED : noir_app.window.pos.y;
    int width = noir_app.window.size.x == 0 ? noir_default_window_size.x : noir_app.window.size.x;
    int height = noir_app.window.size.y == 0 ? noir_default_window_size.y : noir_app.window.size.y;
    SDL_WindowFlags flags = 0;
    if (noir_app.window.resizable) {
        flags |= SDL_WINDOW_RESIZABLE;
    }
    if (noir_app.window.hidden) {
        flags |= SDL_WINDOW_HIDDEN;
    }
    SDL_Window *sdl_window = SDL_CreateWindow(noir_app.window.title, x, y, width, height, flags);
    if (!sdl_window) {
        noir_app.error = "Window creation failed";
        return false;
    }
    noir_app.window.sdl = sdl_window;
    noir_app.window.synced_pos = noir_app.window.pos;
    strncpy(noir_app.window.synced_title, noir_app.window.title, sizeof(noir_app.window.synced_title) - 1);
    update_window();
    return true;
}

static void init_time(void) {
    noir_app.time.ticks_per_sec = SDL_GetPerformanceFrequency();
    noir_app.time.sdl_start_ticks = SDL_GetPerformanceCounter();
}

static bool check_init(void) {
    if (!noir_app.init) {
        noir_app.error = "Not initialized";
        return false;
    }
    return true;
}

static void update_mouse(void) {
    if (noir_app.mouse.capture != noir_app.mouse.synced_capture) {
        if (SDL_CaptureMouse(noir_app.mouse.capture) < 0) {
            sdl_error("SDL_CaptureMouse");
        }
    }
    noir_app.mouse.synced_capture = noir_app.mouse.capture;

    if (noir_app.mouse.pos.x != noir_app.mouse.synced_pos.x || noir_app.mouse.pos.y != noir_app.mouse.synced_pos.y) {
        SDL_WarpMouseInWindow(NULL, noir_app.mouse.pos.x, noir_app.mouse.pos.y);
    }
    uint32 state = SDL_GetMouseState(&noir_app.mouse.pos.x, &noir_app.mouse.pos.y);
    noir_app.mouse.delta_pos = (noir_int2){noir_app.mouse.pos.x - noir_app.mouse.synced_pos.x, noir_app.mouse.pos.y - noir_app.mouse.synced_pos.y};
    noir_app.mouse.moved = noir_app.mouse.delta_pos.x || noir_app.mouse.delta_pos.y;
    noir_app.mouse.synced_pos = noir_app.mouse.pos;

    if (noir_app.mouse.global_pos.x != noir_app.mouse.synced_global_pos.x || noir_app.mouse.global_pos.y != noir_app.mouse.synced_global_pos.y) {
        SDL_WarpMouseGlobal(noir_app.mouse.global_pos.x, noir_app.mouse.global_pos.y);
    }
    SDL_GetGlobalMouseState(&noir_app.mouse.global_pos.x, &noir_app.mouse.global_pos.y);
    noir_app.mouse.global_delta_pos = (noir_int2){noir_app.mouse.global_pos.x - noir_app.mouse.synced_global_pos.x, noir_app.mouse.global_pos.y - noir_app.mouse.synced_global_pos.y};
    noir_app.mouse.global_moved = noir_app.mouse.global_delta_pos.x || noir_app.mouse.global_delta_pos.y;
    noir_app.mouse.synced_global_pos = noir_app.mouse.global_pos;
}

static void update_events(void) {
    for (int key = 0; key < noir_NUM_KEYS; key++) {
        noir_reset_digital_button_events(&noir_app.keys[key]);
    }

    noir_reset_digital_button_events(&noir_app.mouse.left_button);
    noir_reset_digital_button_events(&noir_app.mouse.middle_button);
    noir_reset_digital_button_events(&noir_app.mouse.right_button);

    char *text_ptr = noir_app.text;
    char *text_end = noir_app.text + sizeof(noir_app.text) - 1;

    noir_app.num_events = 0;

    SDL_Event event;
    while (SDL_PollEvent(&event)) {
        switch (event.type) {
        case SDL_MOUSEMOTION: {
            noir_int2 pos = {event.motion.x, event.motion.y};
            noir_int2 delta_pos = {event.motion.xrel, event.motion.yrel};
            noir_push_event(noir_EVENT_MOUSE_MOVE, (noir_EventData){.mouse_move = {.pos = pos, .delta_pos = delta_pos}});
            break;
        }
        case SDL_MOUSEBUTTONDOWN:
        case SDL_MOUSEBUTTONUP: {
            noir_MouseButton button = 0;
            if (event.button.button == SDL_BUTTON_LEFT) {
                noir_update_digital_button(&noir_app.mouse.left_button, event.button.state == SDL_PRESSED);
                button = noir_MOUSE_BUTTON_LEFT;
            } else if (event.button.button == SDL_BUTTON_MIDDLE) {
                noir_update_digital_button(&noir_app.mouse.middle_button, event.button.state == SDL_PRESSED);
                button = noir_MOUSE_BUTTON_MIDDLE;
            } else if (event.button.button == SDL_BUTTON_RIGHT) {
                noir_update_digital_button(&noir_app.mouse.right_button, event.button.state == SDL_PRESSED);
                button = noir_MOUSE_BUTTON_RIGHT;
            }
            if (button) {
                noir_EventKind kind = event.type == SDL_MOUSEBUTTONDOWN ? noir_EVENT_MOUSE_BUTTON_DOWN : noir_EVENT_MOUSE_BUTTON_UP;
                noir_push_event(kind, (noir_EventData){.mouse_button = {.button = button, .pos = {event.button.x, event.button.y}}});
            }
            break;
        }
        case SDL_KEYDOWN:
        case SDL_KEYUP: {
            int key = sdl_scancode_to_noir_key[event.key.keysym.scancode];
            if (key) {
                if (!event.key.repeat) {
                    noir_update_digital_button(&noir_app.keys[key], event.key.state == SDL_PRESSED);
                    noir_update_combination_keys();
                }
                noir_EventKind kind = event.type == SDL_KEYDOWN ? noir_EVENT_KEY_DOWN : noir_EVENT_KEY_UP;
                noir_push_event(kind, (noir_EventData){.key = {.key = key, .repeat = event.key.repeat}});
            }
            break;
        }
        case SDL_TEXTINPUT: {
            const char *str = event.text.text;
            while (*str) {
                if (text_ptr == text_end) {
                    noir_app.error = "Text buffer overflow";
                    break;
                }
                *text_ptr++ = *str++;
            }
            break;
        }
        case SDL_QUIT:
            noir_app.quit = true;
            break;
        }
    }

    *text_ptr = 0;
}

static void update_time(void) {
    uint64 ticks = SDL_GetPerformanceCounter() - noir_app.time.sdl_start_ticks;
    noir_app.time.delta_ticks = (int)(ticks - noir_app.time.ticks);
    noir_app.time.ticks = ticks;

    noir_app.time.nsecs = (noir_app.time.ticks * 1000 * 1000 * 1000) / noir_app.time.ticks_per_sec;
    noir_app.time.usecs = (noir_app.time.ticks * 1000 * 1000) / noir_app.time.ticks_per_sec;
    noir_app.time.msecs = (noir_app.time.ticks * 1000) / noir_app.time.ticks_per_sec;
    noir_app.time.secs = (double)noir_app.time.ticks / (double)noir_app.time.ticks_per_sec;

    noir_app.time.delta_nsecs = (int)((noir_app.time.delta_ticks * 1000 * 1000 * 1000) / noir_app.time.ticks_per_sec);
    noir_app.time.delta_usecs = (int)((noir_app.time.delta_ticks * 1000 * 1000) / noir_app.time.ticks_per_sec);
    noir_app.time.delta_msecs = (int)((noir_app.time.delta_ticks * 1000) / noir_app.time.ticks_per_sec);
    noir_app.time.delta_secs = (float)noir_app.time.delta_ticks / (float)noir_app.time.ticks_per_sec;
}

static void update_window(void) {
    if (noir_app.window.fullscreen != noir_app.window.synced_fullscreen) {
        if (SDL_SetWindowFullscreen(noir_app.window.sdl, noir_app.window.fullscreen ? SDL_WINDOW_FULLSCREEN_DESKTOP : 0) < 0) {
            sdl_error("SDL_SetWindowFullscreen");
        }
        noir_app.window.synced_fullscreen = noir_app.window.fullscreen;
    }

    if (noir_app.window.title != noir_app.window.synced_title && strcmp(noir_app.window.title, noir_app.window.synced_title) != 0) {
        SDL_SetWindowTitle(noir_app.window.sdl, noir_app.window.title);
        strncpy(noir_app.window.synced_title, noir_app.window.title, sizeof(noir_app.window.synced_title));
        noir_app.window.title = noir_app.window.synced_title;
    }

    if (!noir_int2_eq(noir_app.window.pos, noir_app.window.synced_pos)) {
        SDL_SetWindowPosition(noir_app.window.sdl, noir_app.window.pos.x, noir_app.window.pos.y);
    }
    SDL_GetWindowPosition(noir_app.window.sdl, &noir_app.window.pos.x, &noir_app.window.pos.y);
    noir_app.window.moved = noir_app.num_updates == 0 || !noir_int2_eq(noir_app.window.pos, noir_app.window.synced_pos);
    noir_app.window.synced_pos = noir_app.window.pos;

    if (!noir_int2_eq(noir_app.window.size, noir_app.window.synced_size)) {
        SDL_SetWindowSize(noir_app.window.sdl, noir_app.window.size.x, noir_app.window.size.y);
    }
    SDL_GetWindowSize(noir_app.window.sdl, &noir_app.window.size.x, &noir_app.window.size.y);
    noir_app.window.resized = noir_app.num_updates == 0 || !noir_int2_eq(noir_app.window.size, noir_app.window.synced_size);
    noir_app.window.synced_size = noir_app.window.size;

    if (noir_app.window.resizable != noir_app.window.synced_resizable) {
        SDL_SetWindowResizable(noir_app.window.sdl, noir_app.window.resizable);
    }
    noir_app.window.synced_resizable = noir_app.window.resizable;

    if (noir_app.window.hidden != noir_app.window.synced_hidden) {
        if (noir_app.window.hidden) {
            SDL_HideWindow(noir_app.window.sdl);
        } else {
            SDL_ShowWindow(noir_app.window.sdl);
        }
    }
    noir_app.window.synced_hidden = noir_app.window.hidden;
}

void update_clipboard(void) {
    // TODO: Concerned about performance implications for large clipboard data.
    if (noir_app.clipboard != noir_app.synced_clipboard) {
        SDL_free(noir_app.synced_clipboard);
        noir_app.synced_clipboard = SDL_strdup(noir_app.clipboard);
        noir_app.clipboard = noir_app.synced_clipboard;
        SDL_SetClipboardText(noir_app.clipboard);
    } else {
        if (SDL_HasClipboardText()) {
            char *new_clipboard = SDL_GetClipboardText();
            if (!noir_app.synced_clipboard || strcmp(new_clipboard, noir_app.synced_clipboard) != 0) {
                SDL_free(noir_app.synced_clipboard);
                noir_app.synced_clipboard = new_clipboard;
                noir_app.clipboard = noir_app.synced_clipboard;
            } else {
                SDL_free(new_clipboard);
            }
        }
    }
}

void update_audio(void) {
    if (!noir_app.audio.valid) {
        return;
    }
    if (noir_app.audio.synced_play != noir_app.audio.play) {
        SDL_PauseAudioDevice(noir_app.audio.sdl_device, !noir_app.audio.play);
    }
    noir_app.audio.synced_play = noir_app.audio.play;
}

bool noir_app_init(void) {
    if (noir_app.init) {
        return true;
    }
    if (SDL_Init(SDL_INIT_EVERYTHING) < 0) {
        sdl_error("SDL_Init");
        return false;
    }
    if (!init_display()) {
        return false;
    }
    if (!init_window()) {
        return false;
    }
    init_keys();
    init_time();
    init_audio();
    noir_app.platform = SDL_GetPlatform();
    noir_app.init = true;
    return true;
}

bool noir_app_update(void) {
    if (!check_init()) {
        return false;
    }
    if (!noir_app.error) {
        SDL_ClearError();
    }
    SDL_PumpEvents();
    update_events();
    update_window();
    update_time();
    update_mouse();
    update_clipboard();
    update_audio();
    noir_app.num_updates++;
    return !noir_app.quit;
}
