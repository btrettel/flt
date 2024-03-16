#ifndef NDEBUG
#define assert(condition) call assert_(condition, __FILE__, __LINE__)
#define assert_info(condition, info_) call assert_(condition, __FILE__, __LINE__, info=info_)
#else
#define assert(condition, info)
#endif
