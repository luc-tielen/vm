#ifndef UTILS_H
#define UTILS_H

#include <stdio.h>
#include "types.h"

void fprint_stack(FILE* fp, int size, union StackObject* stack);
void fprint_heap(FILE* fp, int size, struct HeapObject** heap);
uint16_t getHeapObjectSize(struct HeapObject* ptr);
uint16_t getHeapInfoSizeInBytes(uint16_t info);
uint16_t getHeapInfoLogicalSize(uint16_t info);

#endif
