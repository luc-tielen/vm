#include <stdio.h>

#define DEBUG 1
#define DEBUG_PRINT(op) \
  printf("(%d) %s | sp: %d | program[ip]: %d | stack: ", ip, (op), sp, program[ip]); \
  print_array(sp + 1, stack);

int interpret(int* program);
void print_array(int size, int* array);

int main(void) {

  // int program[] = { 1, 17, 2, 0 }; // print 17
  // int program[] = { 1, 1, 1, 2, 3, 4, 5, 4, 5, 2, 0 }; // print (1 + 2)
  // print (1 + (2 + (3 + (4 + 5)))))
  int program[] = { 1, 1, 1, 2, 1, 3, 1, 4, 1, 5, 5, 3, 4, 3, 4, 5, 3, 4, 3, 4, 5, 3, 4, 3, 4, 5, 3, 4, 3, 4, 2, 0 };
  return interpret(program);
}

/* bytecodes:

   - 0: Halt
   - 1: Load <int>
   - 2: Print
   - 3: Swap top 2 items on the stack
   - 4: Pop the top item off the stack
   - 5: Add top 2 items on the stack

 */

int interpret(int* program) {
  void* instructions[] = {
    &&halt,
    &&load_int_lit,
    &&print,
    &&swap,
    &&pop,
    &&add
  };

  int stack[1023] = { 0 };
  int sp = -1;
  int ip = 0;

  goto *instructions[program[ip]];

 halt:
  #if DEBUG
  DEBUG_PRINT("halt");
  #endif

  return 0;

 load_int_lit:
  #if DEBUG
  DEBUG_PRINT("load_int_lit");
  #endif

  ++ip;
  stack[++sp] = program[ip];
  goto *instructions[program[++ip]];

 print:
  #if DEBUG
  DEBUG_PRINT("print");
  #endif

  printf("%d\n", stack[sp]);
  goto *instructions[program[++ip]];

 swap:
  #if DEBUG
  DEBUG_PRINT("swap");
  #endif

  int x = stack[sp];
  stack[sp] = stack[sp - 1];
  stack[sp - 1] = x;
  goto *instructions[program[++ip]];

 pop:
  #if DEBUG
  DEBUG_PRINT("pop");
  #endif

  --sp;
  goto *instructions[program[++ip]];

 add:
  #if DEBUG
  DEBUG_PRINT("add");
  #endif

  ++sp;
  stack[sp] = stack[sp - 1] + stack[sp - 2];
  goto *instructions[program[++ip]];


  return 1;
}

void print_array(int size, int* array) {
  for (int i = 0; i < size; ++i) {
    printf("%d ", array[i]);
  }
  puts("\n");
}
