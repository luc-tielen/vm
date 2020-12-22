
#include "types.h"
#include "utils.h"

void fprint_stack(FILE* fp, unsigned int size, StackObject* stack) {
  for (unsigned int i = 0; i < size; ++i) {
    fprint_stackobj(fp, 1, stack[i]);
  }
  fprintf(fp, "\n");
}

void fprint_stackobj(FILE* fp, int verbosity, StackObject stack_obj) {
  if (stack_obj.integer & IS_INTEGER_TAG) {
    fprintf(fp, "%ld ", (stack_obj.integer >> 1));
  } else {
    if (verbosity) {
      fprint_heapobj(fp, stack_obj.pointer);
    } else {
      fprintf(fp, "%lu ", (unsigned long)(stack_obj.pointer));
    }
  }
}

void fprint_heap(FILE* fp, unsigned int size, HeapObject** heap) {
  for (unsigned int i = 0; i < size; ++i) {
    fprintf(fp, "%u) ", i);
    fprint_heapobj(fp, heap[i]);
    fprintf(fp, "\n");
  }
  fprintf(fp, "\n");
}

void fprint_heapobj(FILE* fp, HeapObject* heap_obj) {
  if (heap_obj->info & IS_BYTEARRAY_TAG) {
    fprintf(fp, "[%.*s] ", getHeapObjectSize(heap_obj), ((char*)(heap_obj->data)));
  } else {
    uint16_t size = getHeapInfoLogicalSize(heap_obj->info);

    fprintf(fp, "[ ");
    for (uint16_t i = 0; i < size; ++i) {

      fprint_stackobj(fp, 1, *(StackObject*)(&(heap_obj->data[i*sizeof(StackObject)]))); // wow this is hacky
    }
    fprintf(fp, "] ");

  }
}

uint16_t getHeapObjectSize(HeapObject* ptr) {
  return getHeapInfoSizeInBytes(ptr->info);
}

uint16_t getHeapInfoSizeInBytes(uint16_t info) {
  if (info & IS_BYTEARRAY_TAG) {
    return (info >> 2);
  } else {
    return (info >> 2) * sizeof(StackObject);
  }
}

uint16_t getHeapInfoLogicalSize(uint16_t info) {
  if (info & IS_BYTEARRAY_TAG) {
    return (info >> 2);
  } else {
    return (info >> 2);
  }
}
