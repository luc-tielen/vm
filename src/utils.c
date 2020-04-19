
#include "types.h"
#include "utils.h"

void fprint_stack(FILE* fp, int size, union StackObject* stack) {
  for (int i = 0; i < size; ++i) {
    if (stack[i].integer & 1) {
      fprintf(fp, "%ld ", (stack[i].integer >> 1));
    } else {
      fprintf(fp, "%x ", (unsigned int)(stack[i].pointer));
    }
  }
  fprintf(fp, "\n");
}

void fprint_heap(FILE* fp, int size, HeapObject** heap) {
  for (int i = 0; i < size; ++i) {
    struct HeapObject* temp_ptr = heap[i];
    if (temp_ptr->info & IS_BYTEARRAY_TAG) {
      fprintf(fp, "%d) %.*s\n", i, getHeapObjectSize(temp_ptr), ((char*)(temp_ptr->data)));
    }
  }
  fprintf(fp, "\n");
}

unsigned short getHeapObjectSize(HeapObject* ptr) {
  return getHeapInfoSize(ptr->info);
}

unsigned short getHeapInfoSize(unsigned short info) {
  if (info & IS_BYTEARRAY_TAG) {
    return (info >> 2);
  } else {
    return (info >> 2) * sizeof(StackObject);
  }
}
