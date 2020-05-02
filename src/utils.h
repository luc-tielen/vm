#ifndef UTILS_H
#define UTILS_H

#include <stdio.h>
#include "types.h"

void fprint_stack(FILE* fp, unsigned int size, StackObject* stack);
void fprint_stackobj(FILE* fp, int verbosity, StackObject stack_obj);
void fprint_heap(FILE* fp, unsigned int size, HeapObject** heap_obj);
void fprint_heapobj(FILE* fp, unsigned int index, HeapObject* obj);
uint16_t getHeapObjectSize(HeapObject* ptr);
uint16_t getHeapInfoSizeInBytes(uint16_t info);
uint16_t getHeapInfoLogicalSize(uint16_t info);

#endif
