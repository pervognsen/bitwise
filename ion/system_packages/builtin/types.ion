#foreign(header = "<limits.h>")
#foreign(header = "<stdint.h>")

#static_assert(sizeof(bool) == 1)
#static_assert(sizeof(char) == 1)
#static_assert(sizeof(uchar) == 1)
#static_assert(sizeof(schar) == 1)
#static_assert(sizeof(short) == 2)
#static_assert(sizeof(ushort) == 2)
#static_assert(sizeof(int) == 4)
#static_assert(sizeof(uint) == 4)
#static_assert(sizeof(ullong) == 8)
#static_assert(sizeof(llong) == 8)
#static_assert(sizeof(float) == 4)
#static_assert(sizeof(double) == 8)

#static_assert(alignof(bool) == sizeof(bool))
#static_assert(alignof(char) == sizeof(char))
#static_assert(alignof(uchar) == sizeof(uchar))
#static_assert(alignof(schar) == sizeof(schar))
#static_assert(alignof(short) == sizeof(short))
#static_assert(alignof(ushort) == sizeof(ushort))
#static_assert(alignof(int) == sizeof(int))
#static_assert(alignof(uint) == sizeof(uint))
#static_assert(alignof(long) == sizeof(long))
#static_assert(alignof(ulong) == sizeof(ulong))
#static_assert(alignof(llong) == sizeof(llong))
#static_assert(alignof(ullong) == sizeof(ullong))
#static_assert(alignof(float) == sizeof(float))
#static_assert(alignof(double) == sizeof(double))

@foreign("int8_t")
typedef int8 = schar;

@foreign("uint8_t")
typedef uint8 = uchar;

@foreign("int16_t")
typedef int16 = short;

@foreign("uint16_t")
typedef uint16 = ushort;

@foreign("int32_t")
typedef int32 = int;

@foreign("uint32_t")
typedef uint32 = uint;

@foreign("int64_t")
typedef int64 = llong;

@foreign("uint64_t")
typedef uint64 = ullong;

@foreign
const NULL = (:void*)0;

@foreign
const false = bool(0);

@foreign
const true = bool(1);

const UCHAR_MIN = uchar(0);

@foreign
const UCHAR_MAX = uchar(0xff);

@foreign
const SCHAR_MIN = schar(-128);

@foreign
const SCHAR_MAX = schar(127);

@foreign
const CHAR_MIN = char(SCHAR_MIN);

@foreign
const CHAR_MAX = char(SCHAR_MAX);

@foreign("SHRT_MIN")
const SHORT_MIN = short(-32768);

@foreign("SHRT_MAX")
const SHORT_MAX = short(32767);

const USHORT_MIN = short(0);

@foreign("USHRT_MAX")
const USHORT_MAX = ushort(0xffff);

@foreign
const INT_MIN  = int(-2147483647 - 1);

@foreign
const INT_MAX = int(2147483647);

const UINT_MIN = uint(0);

@foreign
const UINT_MAX = uint(0xffffffff);

@foreign
const LLONG_MIN = llong(-9223372036854775807ll - 1);

@foreign
const LLONG_MAX = llong(9223372036854775807ll);

const ULLONG_MIN = ullong(0);

@foreign
const ULLONG_MAX = ullong(0xffffffffffffffffull);

const UINT8_MIN = UCHAR_MIN;

@foreign
const UINT8_MAX = UCHAR_MAX;

@foreign
const INT8_MIN = SCHAR_MIN;

@foreign
const INT8_MAX = SCHAR_MAX;

const UINT16_MIN = USHORT_MIN;

@foreign
const UINT16_MAX = USHORT_MAX;

@foreign
const INT16_MIN = SHORT_MIN;

@foreign
const INT16_MAX = SHORT_MAX;

const UINT32_MIN = UINT_MIN;

@foreign
const UINT32_MAX = UINT_MAX;

@foreign
const INT32_MIN = INT_MIN;

@foreign
const INT32_MAX = INT_MAX;

const UINT64_MIN = ULLONG_MIN;

@foreign
const UINT64_MAX = ULLONG_MAX;

@foreign
const INT64_MIN = LLONG_MIN;

@foreign
const INT64_MAX = LLONG_MAX;
