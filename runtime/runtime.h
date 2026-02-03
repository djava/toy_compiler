// At several points in our compiler we have decided to rely on the
// fact that pointers are 64 bits wide. The stdint.h header file
// declares a platform aware type that is guaranteed to contain
// 64-bits.
#include <stdint.h>

// Fromspace is our heap which is conceptually an array of 64 bit data
// unless meta information tells us more about about their contents.
extern int64_t* __gc_fromspace_begin;
extern int64_t* __gc_fromspace_end;

// The free pointer should always point to the next free memory
// location. While the mutator (user program) is running this
// should always be pointing into fromspace.
extern int64_t* __gc_free_ptr;

// The root stack is an array of pointers into the heap.  During calls
// to the collector only pointers between the roostack_ptr and
// rootstack_begin are considered as live roots.
extern int64_t** __gc_rootstack_begin;
extern int64_t** __gc_rootstack_end;

// Initialize the memory of the runtime with a fixed rootstack size
// and initial heap size.
void __gc_initialize(uint64_t rootstack_size, uint64_t heap_size);

// Collect garbage data making room for a requested amount of memory.
// Use the pointers in the rootstack to determine what values in the
// heap are still live.
void __gc_collect(int64_t** rootstack_ptr, uint64_t bytes_requested);

// Read an integer from stdin.
int64_t read_int();

// Print an integer to stdout.
void print_int(int64_t x);

// Print a boolean to stdout.
void print_bool(int64_t x);

void print_heap(int64_t** rootstack_ptr);
void print_vector(int64_t* vector_ptr);
void print_vecbegin();
void print_vecend();
void print_space();

void print_ellipsis();
void print_any(int64_t any);




