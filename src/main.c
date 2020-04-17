
#include <stdio.h>
#include "vm.h"

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
