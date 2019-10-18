#include <stdlib.h>
#include <stdio.h>
#include "data.h"

char* project(mystruct *s) {
  return s->bar;
}

mystruct* make(int f, char* s) {
  mystruct* r = (mystruct*)malloc(sizeof (mystruct));
  r->foo = f;
  r->bar = s;
  return r;
}

//int main(int argc, char** argv) {
//  mystruct x = { 1, "baz" };
//  printf("%s", project(x));
//  return 0;
//}
