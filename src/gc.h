#ifndef GC_H
#define GC_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "types.h"

void gen0_gc(struct VM* vm);
void cleanup(struct VM* vm);

#endif
