#include "types.h"
#include "gc.h"
#include "utils.h"

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
  // mark
  for (int i = 0; i < vm->sp; ++i) {
    if (vm->stack[i].integer & 1) {
      break;
    } else {
      // When we'll have heap object that can pointer to other objects, we'll chase these pointers.
      struct HeapObject* temp_ptr = vm->stack[i].pointer;
      temp_ptr->is_marked = 1;
    }
  }
  // sweep / copy
  for (int i = 0; i < GEN0_SIZE; ++i) {
    if (vm->gen0[i]->is_marked) {
      vm->gen0[i]->is_marked = 0;

      if (vm->gen1p < GEN1_SIZE) {
        vm->gen1[vm->gen1p++] = vm->gen0[i];
      } else {
        exit(1); // @TODO: gen1_gc
      }

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


void cleanup(struct VM* vm) {
  for (int i = 0; i < vm->gen0p; ++i) {
    free(vm->gen0[i]);
  }
  for (int i = 0; i < vm->gen1p; ++i) {
    free(vm->gen1[i]);
  }
}
