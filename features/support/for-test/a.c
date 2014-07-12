#include <stdio.h>

int a() {
  printf("%s\n", __FUNCTION__);
  return 0;
}

int main(int argc, char *argv[]) {
  a();
  return 0;
}
