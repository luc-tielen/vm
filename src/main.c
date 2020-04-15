#include <stdio.h>

#define DEBUG 0
#define DEBUG_PRINT(op) \
  fprintf(stderr, "(%d) %s | sp: %d | program[ip]: %d | stack: ", ip, (op), sp, program[ip]); \
  fprint_array(stderr, sp + 1, stack);

int interpret(unsigned char* program);
void fprint_array(FILE* fp, int size, int* array);

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

 */

struct vm {
  unsigned char* program;
  long* stack;
  int sp;
  int ip;

  long temp0;
  long temp1;
};

int interpret(unsigned char* program) {
  void* instructions[] = {
    &&halt,
    &&load_int_lit,
    &&print,
    &&swap,
    &&pop,
    &&add
  };

  long stack[1023] = { 0 };
  struct vm vm;
  vm.program = program;
  vm.stack = stack;
  vm.sp = -1;
  vm.ip = 0;
  
  goto *instructions[vm.program[vm.ip]];

 halt:
  #if DEBUG
  DEBUG_PRINT("halt");
  #endif

  return 0;

 load_int_lit:
  #if DEBUG
  DEBUG_PRINT("load_int_lit");
  #endif

  ++vm.ip;
  stack[++vm.sp] = (long)(vm.program[vm.ip]);
  vm.ip += 8;
  goto *instructions[vm.program[vm.ip]];

 print:
  #if DEBUG
  DEBUG_PRINT("print");
  #endif

  printf("%ld\n", vm.stack[vm.sp]);
  goto *instructions[vm.program[++vm.ip]];

 swap:
  #if DEBUG
  DEBUG_PRINT("swap");
  #endif

  vm.temp0 = vm.stack[vm.sp];
  vm.stack[vm.sp] = vm.stack[vm.sp - 1];
  vm.stack[vm.sp - 1] = vm.temp0;
  goto *instructions[vm.program[++vm.ip]];

 pop:
  #if DEBUG
  DEBUG_PRINT("pop");
  #endif

  --vm.sp;
  goto *instructions[vm.program[++vm.ip]];

 add:
  #if DEBUG
  DEBUG_PRINT("add");
  #endif

  ++vm.sp;
  vm.stack[vm.sp] = vm.stack[vm.sp - 1] + vm.stack[vm.sp - 2];
  goto *instructions[vm.program[++vm.ip]];


  return 1;
}

void fprint_array(FILE* fp, int size, int* array) {
  for (int i = 0; i < size; ++i) {
    fprintf(fp, "%d ", array[i]);
  }
  fprintf(stderr, "\n");
}

// tests


  // int program[] = { 1, 17, 2, 0 }; // print 17
  // int program[] = { 1, 1, 1, 2, 3, 4, 5, 4, 5, 2, 0 }; // print (1 + 2)
  // print (1 + (2 + (3 + (4 + 5)))))
  //int program[] = { 1, 1, 1, 2, 1, 3, 1, 4, 1, 5, 5, 3, 4, 3, 4, 5, 3, 4, 3, 4, 5, 3, 4, 3, 4, 5, 3, 4, 3, 4, 2, 0 };
