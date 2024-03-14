#ifndef NDEBUG
#define assert(arg1, arg2) call assert_(arg1, arg2, __FILE__, __LINE__)
#else
#define assert(arg1, arg2)
#endif
