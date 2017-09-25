#include <R.h>
#include <Rinternals.h>

extern void openblas_set_num_threads (int);

SEXP set_blas_Call (SEXP np) {
  int n = *INTEGER(np);
  openblas_set_num_threads(n);
  return R_NilValue;
}
