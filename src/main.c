#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define DEBUG 0
#define DEBUG_PRINT(op) \
  fprintf(stderr, "(%u) %s | sp: %d | program[ip]: %d | stack: ", vm.ip, (op), vm.sp, vm.program[vm.ip]); \
  fprint_stack(stderr, vm.sp + 1, vm.stack);

#define GEN0_SIZE 3 // We'll keep it small for testing purposes
#define GEN1_SIZE 4095
#define STACK_SIZE 1023

struct HeapObject {
  unsigned char size;
  unsigned char is_marked; // we might decide to mark twice before promoting
  unsigned char data[];
};

union StackObject {
  long integer;
  struct HeapObject* pointer;
};

struct VM {
  unsigned char* program;
  unsigned int ip;

  union StackObject* stack;
  int sp;

  struct HeapObject** gen0;
  int gen0p;
  struct HeapObject** gen1;
  int gen1p;

  long temp0;
  long temp1;

  struct HeapObject* temp_ptr0;
};

void cleanup(struct VM* vm);
void gen0_gc(struct VM* vm);
int interpret(unsigned char* program);
void fprint_stack(FILE* fp, int size, union StackObject* stack);

/*
int main(int argc, char* argv[]) {
  if (argc != 2) {
    puts("Expecting the first argument to be the input file.\n");
    return 2;
  }

  FILE *fp = fopen(argv[argc], "r");
  */

int main(void) {
  FILE *fp = fopen("a.bin", "r");
  unsigned char program[1023];
  fread(program, sizeof(program), 1, fp);
  fclose(fp);

  return interpret(program);
}

/* bytecodes:

   - 0: Halt
   - 1: Load <int>
   - 2: Print
   - 3: Swap top 2 items on the stack
   - 4: Pop the top item off the stack
   - 5: Add top 2 items on the stack
   - 6: Load <string>
   - 7: Print the top item (which is expected to be a string)

 */


int interpret(unsigned char* program) {
  void* instructions[] = {
    &&halt,
    &&load_int_lit,
    &&print,
    &&swap,
    &&pop,
    &&add,
    &&load_str_lit,
    &&print_str
  };

  unsigned char length = 0;
  struct HeapObject* gen0[GEN0_SIZE];
  struct HeapObject* gen1[GEN1_SIZE];
  union StackObject stack[STACK_SIZE] = { {0} };
  struct VM vm;
  vm.program = program;
  vm.ip = 0;
  vm.stack = stack;
  vm.sp = -1;

  vm.gen0 = gen0;
  vm.gen0p = 0;
  vm.gen1 = gen1;
  vm.gen1p = 0;

  goto *instructions[vm.program[vm.ip]];

 halt:
  #if DEBUG
  DEBUG_PRINT("halt");
  #endif

  cleanup(&vm);
  return 0;

 load_int_lit:
  #if DEBUG
  DEBUG_PRINT("load_int_lit");
  #endif

  ++vm.ip;
  stack[++vm.sp].integer = (long)(vm.program[vm.ip]);
  vm.ip += 8;
  goto *instructions[vm.program[vm.ip]];

 load_str_lit:
  #if DEBUG
  DEBUG_PRINT("load_str_lit");
  #endif

  ++vm.ip;
  length = vm.program[vm.ip++];
  vm.temp_ptr0 = malloc(sizeof(unsigned char) + sizeof(unsigned char) + length); // size + is_marked + data
  vm.temp_ptr0->size = length;
  vm.temp_ptr0->is_marked = 0;
  memcpy(vm.temp_ptr0->data, &(vm.program[vm.ip]), length);
  stack[++vm.sp].pointer = vm.temp_ptr0;

  if (vm.gen0p >= GEN0_SIZE) {
    gen0_gc(&vm);
  }
  gen0[vm.gen0p++] = vm.temp_ptr0;

  vm.ip += length;
  goto *instructions[vm.program[vm.ip]];

 print:
  #if DEBUG
  DEBUG_PRINT("print");
  #endif

  printf("%ld\n", (vm.stack[vm.sp].integer >> 1)); // to remove the least significant bit
  goto *instructions[vm.program[++vm.ip]];

 print_str:
  #if DEBUG
  DEBUG_PRINT("print_str");
  #endif

  vm.temp_ptr0 = vm.stack[vm.sp].pointer;
  printf("%.*s\n", vm.temp_ptr0->size, ((char*)(vm.temp_ptr0->data)));

  goto *instructions[vm.program[++vm.ip]];

 swap:
  #if DEBUG
  DEBUG_PRINT("swap");
  #endif

  vm.temp0 = vm.stack[vm.sp].integer;
  vm.stack[vm.sp] = vm.stack[vm.sp - 1];
  vm.stack[vm.sp - 1].integer = vm.temp0;
  goto *instructions[vm.program[++vm.ip]];

 pop:
  #if DEBUG
  DEBUG_PRINT("pop");
  #endif

  vm.stack[vm.sp].pointer = 0;
  --vm.sp;
  goto *instructions[vm.program[++vm.ip]];

 add:
  #if DEBUG
  DEBUG_PRINT("add");
  #endif

  ++vm.sp;
  vm.stack[vm.sp].integer = (((vm.stack[vm.sp - 1].integer >> 1) + (vm.stack[vm.sp - 2].integer >> 1)) << 1) + 1;
  goto *instructions[vm.program[++vm.ip]];


  return 1;
}

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
