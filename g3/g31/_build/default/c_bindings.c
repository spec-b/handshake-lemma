#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <stdio.h>
#include <stdlib.h>

CAMLprim value simulate_heavy_check(value vsize) {
  CAMLparam1(vsize);
  int size = Int_val(vsize);
  int *array = malloc(sizeof(int) * size * 10000);
  if (!array) CAMLreturn(Val_bool(0));
  for (int i = 0; i < size * 10000; i++) {
    array[i] = i % 7;
  }
  long sum = 0;
  for (int i = 0; i < size * 10000; i++) {
    sum += array[i];
  }
  free(array);
  CAMLreturn(Val_bool(sum > 0));
}
