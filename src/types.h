#ifndef TYPES_H
#define TYPES_H

#define DEBUG 0

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

#endif
