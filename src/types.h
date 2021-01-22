#ifndef TYPES_H
#define TYPES_H

#include <stdint.h>
#include <stdbool.h>

#define DEBUG 0
#define USE_ASSERTS 1

#define PROGRAM_SIZE 1024*1024

#define GEN0_SIZE 4 // We'll keep it small for testing purposes
#define GEN1_SIZE 32
#define STACK_SIZE 1024

// For heap objects
#define GC_MARKED_TAG 0b1
#define HEAPARRAY_TAG 0b000
#define BYTEARRAY_TAG 0b001
#define CLOSURE_TAG 0b010

// For stack objects
#define INTEGER_TAG 0b1
#define POINTER_TAG 0b0


typedef struct HeapObject {
  uint16_t info;
  uint8_t  data[]; // could be either bytearray or multiple stack objects
} HeapObject;

typedef union StackObject {
  long integer;
  struct HeapObject* pointer;
} StackObject;

struct VM {
  uint8_t* program;
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

bool is_integer(StackObject obj);
bool is_pointer(StackObject obj);

uint8_t get_heap_object_tag(HeapObject* obj);
uint8_t get_stack_object_tag(StackObject* obj);

bool is_heap_array(HeapObject* obj);
bool is_bytearray(HeapObject* obj);
bool is_closure(HeapObject* obj);

uint8_t get_closure_argsize(HeapObject* obj);
uint8_t get_closure_appliednum(HeapObject* obj);

bool is_gc_marked(HeapObject* obj);
void clear_gc_marked(HeapObject* obj);
void set_gc_marked(HeapObject* obj);

#endif
