#declare_note(foreign)
#declare_note(static_assert)
#declare_note(always)
#declare_note(threadlocal)

#always

#foreign(preamble="""#define __USE_MINGW_ANSI_STDIO 1
#ifndef _CRT_SECURE_NO_WARNINGS
#define _CRT_SECURE_NO_WARNINGS
#endif
#ifndef _CRT_NONSTDC_NO_DEPRECATE
#define _CRT_NONSTDC_NO_DEPRECATE
#endif

#if _MSC_VER >= 1900 || __STDC_VERSION__ >= 201112L
// Visual Studio 2015 supports enough C99/C11 features for us.
#else
#error C11 support required or Visual Studio 2015 or later
#endif

#if _MSC_VER
#define THREADLOCAL __declspec(thread)
#define INLINE static inline __forceinline
#define NOINLINE __declspec(noinline)
#endif

#if __GNUC__
#define THREADLOCAL __thread
#define INLINE static inline __attribute__((always_inline))
#define NOINLINE __attribute__((noinline))
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wvarargs"
#endif

#include <stdbool.h>
#include <stdint.h>
#include <stddef.h>
#include <stdarg.h>
#include <assert.h>
#include <string.h>
#include <stdlib.h>

typedef unsigned char uchar;
typedef signed char schar;
typedef unsigned short ushort;
typedef unsigned int uint;
typedef unsigned long ulong;
typedef long long llong;
typedef unsigned long long ullong;

#ifdef _MSC_VER
#define alignof(x) __alignof(x)
#else
#define alignof(x) __alignof__(x)
#endif

typedef void *(*AllocFunc)(void *data, size_t size, size_t align);
typedef void (*FreeFunc)(void *data, void *ptr);

typedef struct Allocator {
    AllocFunc alloc;
    FreeFunc free;
} Allocator;

INLINE
void *default_alloc(void *allocator, size_t size, size_t align) {
    // todo: use _aligned_malloc, etc
    return malloc(size);
}

INLINE
void default_free(void *allocator, void *ptr) {
    // todo: use _aligned_free, etc
    free(ptr);
}

THREADLOCAL
Allocator *current_allocator = &(Allocator){default_alloc, default_free};

INLINE
void *generic_alloc(Allocator *allocator, size_t size, size_t align) {
    if (!size) {
        return 0;
    }
    if (!allocator) {
        allocator = current_allocator;
    } 
    return allocator->alloc(allocator, size, align);
}

INLINE
void generic_free(Allocator *allocator, void *ptr) {
    if (!allocator) {
        allocator = current_allocator;
    } 
    allocator->free(allocator, ptr);
}

INLINE
void *generic_alloc_copy(Allocator *allocator, size_t size, size_t align, const void *src) {
    if (!allocator) {
        allocator = current_allocator;
    } 
    void *ptr = allocator->alloc(allocator, size, align);
    if (!ptr) {
        return 0;
    }
    memcpy(ptr, src, size);
    return ptr;
}

#define tls_alloc(size, align) (generic_alloc(current_allocator, (size), (align)))
#define tls_free(ptr) (generic_free(current_allocator, (ptr)))
#define alloc_copy(size, align, src) (generic_alloc_copy(current_allocator, (size), (align), (src)))
""")

#foreign(postamble=
"""
#ifdef __GNUC__
#pragma GCC diagnostic pop
#endif
""")

