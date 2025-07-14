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
void cfree(void *ptr);

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

    /// keep explicit track of the start of the first block using a static variable
    /// static variables will live for as long as the program is alive unlike
    /// stack allocated variables where they are destroyed when leaving scope
    free_list_head = first;

    /// lets allocate some memory! 
    size_t size = 32;
    void *ptr = cmalloc(size);
    assert(ptr != NULL);

    ((char *)ptr)[0] = 'A';
    ((char *)ptr)[1] = 'B';

    assert(((char *)ptr)[0] == 'A');
    assert(((char *)ptr)[1] == 'B');

    /// test to see if the block was marked as free
    void *freed_ptr = ptr;
    cfree(ptr);
    Block *current = free_list_head;
    while (current != NULL) {
        if (((char *)current + BLOCK_HEADER_SIZE) == freed_ptr) {
            assert(current->free == 1);
            break;
        }
        current = current->next;
    }

    return EXIT_SUCCESS;
}

void cfree(void *ptr)
{
    if (ptr == NULL) {
        return;
    }

    Block *b = (Block *)((char *)ptr - BLOCK_HEADER_SIZE);
    b->free = 1;

    /// now that the given block has been marked free, its time to
    /// merge adjacent free blocks to minimize external fragmentation within
    /// our managed heap, this is called coalescing 
    Block *current = free_list_head;
    while (current != NULL && current->next != NULL) {
        /// are both current and next free? 
        if (current->free == 1 && current->next->free == 1) {
            /// are the blocks physically next to each other in memory?
            if ((char *)current + current->size == (char *)current->next) {
                /// merge the adjacent blocks together
                current->size += current->next->size;
                current->next = current->next->next;
            } else {
                current = current->next;
            }
        } else {
            current = current->next;
        }
    }
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
    start = (char *)mmap(NULL, requested_size, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    if (start == MAP_FAILED) {     
        perror("mmap failed");
        exit(EXIT_FAILURE);
    }

    middle = start + (requested_size / 2);       /// this will give us the middle of our allocated heap  
    end = start + requested_size;                /// this will give us the a pointer to the end of the heap
  
    Heap h = {start, middle, end, requested_size};
    return h;
}