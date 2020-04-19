#ifndef TYPES_H
#define TYPES_H

#define DEBUG 0

#define GEN0_SIZE 4 // We'll keep it small for testing purposes
#define GEN1_SIZE 32
#define STACK_SIZE 1024

#define IS_MARKED 1
#define IS_BYTEARRAY_TAG 2

typedef struct HeapObject {
  unsigned short info;
  unsigned char data[]; // could be either bytearray or multiple stack objects
} HeapObject;

typedef union StackObject {
  long integer;
  struct HeapObject* pointer;
} StackObject;

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
