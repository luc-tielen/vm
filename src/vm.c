#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "types.h"
#include "vm.h"
#include "gc.h"
#include "utils.h"


#define DEBUG_PRINT(op) \
  fprintf(stderr, "(%u) %s | sp: %d | program[ip]: %d | stack: ", vm.ip, (op), vm.sp, vm.program[vm.ip]); \
  fprint_stack(stderr, vm.sp + 1, vm.stack);


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


