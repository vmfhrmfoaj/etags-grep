#include <stdio.h>

int a() {
  printf("%s\n", __FUNCTION__);
  return 0;
}

int a(int b) {
  printf("%s with %d\n", __FUNCTION__, b);
  return 0;
}

int main(int argc, char *argv[]) {
  a();
  a(10);
  return 0;
}
