#include "types.h"

// Stack Objects //

bool is_integer(StackObject obj) {
  return (obj.integer & INTEGER_TAG) > 0;
}
bool is_pointer(StackObject obj) {
  return (obj.integer & POINTER_TAG) == 0;
}

// Heap Objects //

uint8_t get_heap_object_tag(HeapObject* obj) {
  return (obj->info >> 1) & 0b111;
}

bool is_bytearray(HeapObject* obj) {
  return (get_heap_object_tag(obj) & BYTEARRAY_TAG) > 0;
}
bool is_heap_array(HeapObject* obj) {
  return (get_heap_object_tag(obj) & HEAPARRAY_TAG) > 0;
}
bool is_closure(HeapObject* obj) {
  return (get_heap_object_tag(obj) & CLOSURE_TAG) > 0;
}

// Closures

uint8_t get_closure_argsize(HeapObject* obj) {
  return obj->info >> 10;
}

uint8_t get_closure_applied(HeapObject* obj) {
  return (obj->info >> 4) & 0b111111;
}

// GC

bool is_gc_marked(HeapObject* obj) {
  return (obj->info & GC_MARKED_TAG) > 0;
}

void clear_gc_marked(HeapObject* obj) {
  obj->info = (obj->info >> 1) << 1;
}

void set_gc_marked(HeapObject* obj) {
  obj->info |= GC_MARKED_TAG;
}
