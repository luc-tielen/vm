
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

void fprint_heap(FILE* fp, int size, struct HeapObject** heap) {
  for (int i = 0; i < size; ++i) {
    struct HeapObject* temp_ptr = heap[i];
    fprintf(fp, "%.*s\n", temp_ptr->size, ((char*)(temp_ptr->data)));
  }
  fprintf(fp, "\n");
}
