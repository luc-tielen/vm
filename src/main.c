#include <stdio.h>

#define DEBUG 1
#define DEBUG_PRINT(op) \
  printf("(%d) %s | sp: %d | program[ip]: %d\n", ip, (op), sp, program[ip]);

int interpret(int* program);

int main(void) {
  int program[] = { 1, 17, 2, 0 };
  return interpret(program);
}

/* bytecodes:

   - 0: Halt
   - 1: Load <int>
   - 2: Print

 */

int interpret(int* program) {
  void* instructions[] = {
    &&halt,
    &&load_int_lit,
    &&print
  };

  int stack[1023] = { 0 };
  int sp = -1;
  int ip = 0;

  goto *instructions[program[ip++]];

 halt:
  #if DEBUG
  DEBUG_PRINT("halt");
  #endif

  return 0;

 load_int_lit:
  #if DEBUG
  DEBUG_PRINT("load_int_lit");
  #endif

  stack[++sp] = program[ip];
  goto *instructions[program[++ip]];

 print:
  #if DEBUG
  DEBUG_PRINT("print");
  #endif

  printf("%d\n", stack[sp]);
  goto *instructions[program[++ip]];

  return 1;
}
