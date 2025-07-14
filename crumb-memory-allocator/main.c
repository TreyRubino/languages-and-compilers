/// @author Trey Rubino
/// @date   07/12/2025

#include <unistd.h> 
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <sys/mman.h>

#include "inc/heap.h"
#include "inc/block.h"

#define HEAP_SIZE 64 * 1024
#define BLOCK_HEADER_SIZE sizeof(Block)
#define BLOCK_MIN_SIZE sizeof(BLOCK_HEADER_SIZE + 16)

Heap allocate_heap(int requested_size);

/// uses first-fit
void *cmalloc(size_t size);

static Block *free_list_head = NULL; 

int main(int argc, char *argv[])
{    
    Heap heap = allocate_heap(HEAP_SIZE);

    printf("Heap start (the break): %p\n", heap.start);
    printf("Heap middle: %p\n", heap.middle);
    printf("Heap end: %p\n", heap.end); 
    printf("Heap size: %d\n", heap.size);

    /// lets init the first block instance
    Block *first = (Block *)heap.start;
    first->size = heap.size;
    first->free = 1;
    first->next = NULL;

    // keep explicit track of the start of the first block using a static variable
    /// static variables will live for as long as the program is alive unlike
    /// stack allocated variables where they are destroyed when leaving scope
    free_list_head = first;

    /// lets allocate some memory! 
    size_t size = 32;
    void *p = cmalloc(size);
    assert(p != NULL);

    ((char *)p)[0] = 'A';
    ((char *)p)[1] = 'B';

    assert(((char *)p)[0] == 'A');
    assert(((char *)p)[1] == 'B');

    return EXIT_SUCCESS;
}

void *cmalloc(size_t size)
{
    Block *current = free_list_head;
    while (current != NULL) {
        /// if the current block available and is it big enough to handle the requested
        /// allocation space plus the header size
        if (current->free == 1 && current->size >= (size + BLOCK_HEADER_SIZE)) {
            /// is the block big enough to handle a split? 
            if (current->size >= (size + BLOCK_HEADER_SIZE + BLOCK_MIN_SIZE)) {
                /// move forward in memory by the header size and requested payload
                /// this starts to form the left off block after the current becomes allocated
                /// this defines the starting point of that left over block
                /// this is just moving addresses no sizes calculation 
                Block *b = (Block *)((char *)current + BLOCK_HEADER_SIZE + size);    

                /// carve off the size of the allocating block (current)
                /// this sets the size of the left over block to that 
                /// of the current block minus the header minus the allocating size
                b->size = current->size - BLOCK_HEADER_SIZE - size;
                b->free = 1;
                b->next = current->next;

                /// define the size of the allocated block which is trival
                /// requested size plus the defined header size of the block struct
                /// this shrinks the current block to exactly the requested size 
                /// and takes in account the header size as well
                current->size = size + BLOCK_HEADER_SIZE;
                current->free = 0;
                current->next = b;
                return (void *)((char *)current + BLOCK_HEADER_SIZE);
            } else { 
                /// block can't handle a split, return the pointer to the full blocks payload
                return (void *)((char *)current + BLOCK_HEADER_SIZE);
            }
        } else { 
            /// block is not big enough for size plus header, move to the next block and loop
            current = current->next;
        }
    }
    return NULL; /// no block was found for the requested size 
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