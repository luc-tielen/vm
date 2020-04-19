#ifndef UTILS_H
#define UTILS_H

#include <stdio.h>
#include "types.h"

void fprint_stack(FILE* fp, int size, union StackObject* stack);
void fprint_heap(FILE* fp, int size, struct HeapObject** heap);
unsigned short getHeapObjectSize(struct HeapObject* ptr);
unsigned short getHeapInfoSizeInBytes(unsigned short info);
unsigned short getHeapInfoLogicalSize(unsigned short info);

#endif
