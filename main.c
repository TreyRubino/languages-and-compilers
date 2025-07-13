/// @author Trey Rubino
/// @date   07/12/2025

#include <unistd.h> 
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <sys/mman.h>

#define HEAP_SIZE 64 * 1024

typedef struct {
    char *start;
    char *middle;
    char *end; 
    int size;
} Heap;

Heap allocate_heap(int requested_size);

int main(int argc, char *argv[])
{
    Heap heap = allocate_heap(HEAP_SIZE);

    printf("Heap start (the break): %p\n", heap.start);
    printf("Heap middle: %p\n", heap.middle);
    printf("Heap end: %p\n", heap.end); 
    printf("Heap size: %d\n", heap.size);

    /// now, I would like to test to make sure that the heap is writable and readable
    /// first, lets make sure that the memory is writable

    return EXIT_SUCCESS;
}

Heap allocate_heap(int requested_size)
{
    char *start, *middle, *end;

    /// unlike brk(), sbrk returns a pointer to the start of the requested heap
    /// for manipulation up to grow, and down to shrink the allocated heap
    start = (char *)mmap(NULL, requested_size, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    if (start == MAP_FAILED) {      /// returns negative 1 on failure and also sets error numbers
        perror("mmap failed");
        exit(EXIT_FAILURE);
    }

    middle = start + (requested_size / 2);       /// this will give us the middle of our allocated heap  
    end = start + requested_size;                /// this will give us the a pointer to the end of the heap
  
    Heap h = {start, middle, end, requested_size};
    return h;
}