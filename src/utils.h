#ifndef UTILS_H
#define UTILS_H

#include <stdio.h>
#include "types.h"

void fprint_stack(FILE* fp, int size, union StackObject* stack);
void fprint_heap(FILE* fp, int size, struct HeapObject** heap);

#endif
