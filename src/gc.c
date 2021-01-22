#include "types.h"
#include "gc.h"
#include "utils.h"

void mark(struct VM* vm);

void cleanup(struct VM* vm) {
  for (int i = 0; i < vm->gen0p; ++i) {
    free(vm->gen0[i]);
  }
  for (int i = 0; i < vm->gen1p; ++i) {
    free(vm->gen1[i]);
  }
}


/* Stop the world mark and sweep copying garbage collector

 Mark: We walk the stack and will mark each pointer element we reached
 Sweep/Copy: We walk the gen0 heap and:
   - Move each marked pointer to gen1
   - Free each unmarked pointer
 */
void gen0_gc(struct VM* vm) {
  if (DEBUG) {
    fprintf(stderr, "----------------\n");
    fprintf(stderr, "* gen0_gc start \n");
    fprintf(stderr, "----------------\n");
    fprint_heap(stderr, GEN0_SIZE, vm->gen0);
  }
  mark(vm);

  // sweep / copy
  for (int i = 0; i < GEN0_SIZE; ++i) {
    if (is_gc_marked(vm->gen0[i])) {
      clear_gc_marked(vm->gen0[i]);

      if (vm->gen1p >= GEN1_SIZE) {
        gen1_gc(vm);
      }
      vm->gen1[vm->gen1p++] = vm->gen0[i];

    } else {
      free(vm->gen0[i]);
    }

    vm->gen0[i] = 0;
  }

  vm->gen0p = 0;

  if (DEBUG) {
    fprintf(stderr, "--------\n");
    fprintf(stderr, "* gen1: \n");
    fprintf(stderr, "--------\n");
    fprint_heap(stderr, vm->gen1p, vm->gen1);
    fprintf(stderr, "---------------\n");
    fprintf(stderr, "* gen0_gc done \n");
    fprintf(stderr, "---------------\n");
  }
}

// We get here after a marking phase has already been done,
// Should we make should the "is_marked" field is clean before
// marking, or should we start again?
// Marking again is more work, but it means less work for gen0 gc
// which happens more often.
// Let's do another marking for now.
void gen1_gc(struct VM* vm) {
  for (int i = 0; i < GEN1_SIZE; ++i) {
    // clear is_marked - @TODO refactor
    clear_gc_marked(vm->gen1[i]);
  }
  mark(vm);

  if (DEBUG) {
    fprintf(stderr, "----------------\n");
    fprintf(stderr, "* gen1_gc start \n");
    fprintf(stderr, "----------------\n");
    fprint_heap(stderr, GEN1_SIZE, vm->gen1);
  }

  // Let's do something slow for now and allocate a new gen1 array a move
  // all live pointers there, and then back to the original gen1 array.
  struct HeapObject* gen1_temp[GEN1_SIZE] = { 0 };
  int gen1p_temp = 0;

  // copy / sweep
  for (int i = 0; i < GEN1_SIZE; ++i) {
    if (is_gc_marked(vm->gen1[i])) {
        gen1_temp[gen1p_temp++] = vm->gen1[i];
    } else {
      free(vm->gen1[i]);
    }
  }

  if (gen1p_temp >= GEN1_SIZE) {
    fprintf(stderr, "Out of memory.\n");
    exit(17);
  }

  // copy back to original
  for (int i = 0; i < GEN1_SIZE; ++i) {
    vm->gen1[i] = gen1_temp[i];
  }
  vm->gen1p = gen1p_temp;

  if (DEBUG) {
    fprintf(stderr, "--------\n");
    fprintf(stderr, "* gen1: \n");
    fprintf(stderr, "--------\n");
    fprint_heap(stderr, vm->gen1p, vm->gen1);
    fprintf(stderr, "---------------\n");
    fprintf(stderr, "* gen1_gc done \n");
    fprintf(stderr, "---------------\n");
  }
}

// Recursively mark live heap objects
// We could probably find a faster way to do this but this is enough for now
void mark_pointer(HeapObject* obj) {
  if (is_gc_marked(obj)) {
    return;
  }
  set_gc_marked(obj);
  if (is_bytearray(obj)) {
    return;
  }
  uint16_t size = getHeapInfoLogicalSize(obj->info);

  StackObject data;
  for (uint16_t i = 0; i < size; ++i) {
    data = *(StackObject*)(&(obj->data[i*sizeof(StackObject)]));
    if (is_integer(data)) {
    } else {
      mark_pointer(data.pointer);
    }
  }
}

void mark(struct VM* vm) {
  // mark
  for (int i = 0; i < vm->sp; ++i) {
    if (is_integer(vm->stack[i])) {
      continue;
    } else {
      // When we'll have heap object that can pointer to other objects, we'll chase these pointers.
      mark_pointer(vm->stack[i].pointer);
    }
  }
}
