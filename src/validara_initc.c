#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME:
Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP _validaRA_converter_ra_(SEXP);
extern SEXP _validaRA_generate_digit(SEXP, SEXP);
extern SEXP _validaRA_valida_ra(SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
  {"_validaRA_converter_ra_",  (DL_FUNC) &_validaRA_converter_ra_,  1},
  {"_validaRA_generate_digit", (DL_FUNC) &_validaRA_generate_digit, 2},
  {"_validaRA_valida_ra",      (DL_FUNC) &_validaRA_valida_ra,      3},
  {NULL, NULL, 0}
};

void R_init_validaRA(DllInfo *dll)
{
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, TRUE);
}
