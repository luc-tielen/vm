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

#define ASSERT(name, test, excode)              \
  if (!(test)) {                                \
    fprintf(stderr, "Failed test: %s", name);   \
    exit(excode);                               \
  }

#define ASSERT_STACK_SIZE(expected_items, name, vm)                 \
  if (vm.sp + 1 - expected_items < 0) {                             \
    fprintf(stderr, "Stack underflow in: (%d) %s", vm.ip, name);    \
    exit(111);                                                      \
  }

/* opcodes:

   - 0: Halt
   - 1: Load <int>
   - 2: Print
   - 3: Swap top 2 items on the stack
   - 4: Pop the top item off the stack
   - 5: Add top 2 items on the stack
   - 6: Load <string>
   - 7: Print the top item (which is expected to be a string)
   - 8: Cons <size> will create a new heap object and will load
        the last <size> elements on the stack
   - 9: Index <index> will put the <index> element in the
        heap object on the stack

 */


int interpret(uint8_t* program) {
  void* instructions[] = {
    &&halt,
    &&load_int_lit,
    &&print,
    &&swap,
    &&pop,
    &&add,
    &&load_str_lit,
    &&print_str,
    &&cons,
    &&heap_index
  };

  uint16_t heap_info = 0;
  uint16_t size_in_bytes = 0;
  uint16_t logical_size = 0;
  HeapObject* gen0[GEN0_SIZE];
  HeapObject* gen1[GEN1_SIZE];
  StackObject stack[STACK_SIZE] = { {0} };
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
  vm.stack[++vm.sp].integer = (long)(vm.program[vm.ip]);
  vm.ip += 8;
  goto *instructions[vm.program[vm.ip]];

 load_str_lit:
  #if DEBUG
  DEBUG_PRINT("load_str_lit");
  #endif

  ++vm.ip;
  heap_info = vm.program[vm.ip++];
  size_in_bytes = getHeapInfoSizeInBytes(heap_info);
  vm.temp_ptr0 = malloc(sizeof(uint16_t) + size_in_bytes); // info + data
  vm.temp_ptr0->info = heap_info;
  memcpy(vm.temp_ptr0->data, &(vm.program[++vm.ip]), size_in_bytes);
  vm.stack[++vm.sp].pointer = vm.temp_ptr0;

  if (vm.gen0p >= GEN0_SIZE) {
    gen0_gc(&vm);
  }
  gen0[vm.gen0p++] = vm.temp_ptr0;

  vm.ip += size_in_bytes;
  goto *instructions[vm.program[vm.ip]];

 print:
  #if DEBUG
  DEBUG_PRINT("print");
  #endif

  #if USE_ASSERTS
  ASSERT_STACK_SIZE(1, "print", vm);
  #endif

  printf("%ld\n", (vm.stack[vm.sp].integer >> 1)); // to remove the least significant bit
  goto *instructions[vm.program[++vm.ip]];

 print_str:
  #if DEBUG
  DEBUG_PRINT("print_str");
  #endif

  #if USE_ASSERTS
  ASSERT_STACK_SIZE(1, "print_str", vm);
  #endif

  vm.temp_ptr0 = vm.stack[vm.sp].pointer;
  printf("%.*s\n", getHeapObjectSize(vm.temp_ptr0), ((char*)(vm.temp_ptr0->data)));

  goto *instructions[vm.program[++vm.ip]];

 swap:
  #if DEBUG
  DEBUG_PRINT("swap");
  #endif

  #if USE_ASSERTS
  ASSERT_STACK_SIZE(2, "swap", vm);
  #endif

  vm.temp0 = vm.stack[vm.sp].integer;
  vm.stack[vm.sp] = vm.stack[vm.sp - 1];
  vm.stack[vm.sp - 1].integer = vm.temp0;
  goto *instructions[vm.program[++vm.ip]];

 pop:
  #if DEBUG
  DEBUG_PRINT("pop");
  #endif

  #if USE_ASSERTS
  ASSERT_STACK_SIZE(1, "pop", vm);
  #endif

  vm.stack[vm.sp].pointer = 0;
  --vm.sp;
  goto *instructions[vm.program[++vm.ip]];

 add:
  #if DEBUG
  DEBUG_PRINT("add");
  #endif

  #if USE_ASSERTS
  ASSERT_STACK_SIZE(2, "add", vm);
  #endif

  ++vm.sp;
  vm.stack[vm.sp].integer = (((vm.stack[vm.sp - 1].integer >> 1) + (vm.stack[vm.sp - 2].integer >> 1)) << 1) + 1;
  goto *instructions[vm.program[++vm.ip]];


  // 8: Cons <size> will create a new heap object and will load
  //    the last <size> elements on the stack
 cons:
  #if DEBUG
  DEBUG_PRINT("cons");
  #endif

  ++vm.ip;
  heap_info = vm.program[vm.ip++];
  ++vm.ip;

  size_in_bytes = getHeapInfoSizeInBytes(heap_info);
  logical_size = getHeapInfoLogicalSize(heap_info);

  #if USE_ASSERTS
  ASSERT_STACK_SIZE(logical_size, "cons", vm);
  #endif

  vm.temp_ptr0 = malloc(sizeof(uint16_t) + size_in_bytes); // info + data
  vm.temp_ptr0->info = heap_info;
  memcpy(vm.temp_ptr0->data, &(vm.stack[vm.sp - logical_size + 1]), size_in_bytes);
  vm.stack[++vm.sp].pointer = vm.temp_ptr0;

  if (vm.gen0p >= GEN0_SIZE) {
    gen0_gc(&vm);
  }
  gen0[vm.gen0p++] = vm.temp_ptr0;

  goto *instructions[vm.program[vm.ip]];


  // - 9: Index <index> will put the <index> element in the
  //      heap object on the stack
 heap_index:
  #if DEBUG
  DEBUG_PRINT("heap_index");
  #endif

  #if USE_ASSERTS
  ASSERT_STACK_SIZE(1, "heap_index", vm);
  #endif

  ++vm.ip;
  heap_info = vm.program[vm.ip++];
  logical_size = getHeapInfoLogicalSize(heap_info);
  ++vm.ip;
  ++vm.sp;

  vm.stack[vm.sp] = *(StackObject*)(&(vm.stack[vm.sp - 1].pointer->data[logical_size * sizeof(StackObject)])); // again, quite hacky.

  goto *instructions[vm.program[vm.ip]];


  // we don't expect to reach here. We expect HALT to be the last instruction
  return 1;
}


