#include "stdio.h"
#include "stdlib.h"
#include "limits.h"
#include "errno.h"
#include "string.h"

int main(int argc, char* argv[argc]) {

  if (argc != 2) {
    fprintf(stderr, "Usage: %s <input>\n", argv[0]);
    return 1;
  }

  FILE* file = fopen(argv[1], "r");
  if (file == NULL) {
    fprintf(stderr, "Could not open file: \"%s\". %s.\n", argv[1], strerror(errno));
    return 1;
  }

  int previous_value = INT_MAX, value = 0;
  int count = 0;
  while (fscanf(file, "%d", &value) > 0) {
    if (value > previous_value) count++;
    previous_value = value;
  }

  printf("%d\n", count);
  fclose(file);
  return 0;
}