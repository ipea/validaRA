#include "libraries.h"
#include "convert2int.h"
#include <Rcpp.h>
#include <algorithm>
#include "utils.h"
#include <string>
#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>

// [[Rcpp::depends(BH)]]
// [[Rcpp::export]]
SEXP valida_ra(SEXP x, SEXP type, SEXP log){
  SEXP r = PROTECT(Rf_allocVector(LGLSXP, LENGTH(x)));
  SEXP l = PROTECT(Rf_allocVector(INTSXP, LENGTH(x)));
  const char *t = CHAR(STRING_ELT(type,0));

  // Utilização do ponteiro original (raw pointer).
  // Evita o destrutor automático que causava crash no RStudio (Heap Corruption)
  Ra *ra = factoryRa(t);

  if(is_factor(x)){
    // CORREÇÃO CRÍTICA PARA O CRAN: Fatores são INTSXP internamente.
    // É obrigatório coagir para STRSXP antes de usar STRING_ELT.
    SEXP x_str = PROTECT(Rf_coerceVector(x, STRSXP));

    for(int i = 0; i < LENGTH(x_str); i++){
      ra->set_digits(charxp2arrayint(STRING_ELT(x_str, i), ra->get_size()));
      LOGICAL(r)[i] = ra->validate_generic();
      INTEGER(l)[i] = ra->get_error();
      ra->clear();
    }
    UNPROTECT(1); // Desprotege o x_str

  }else if(TYPEOF(x) == STRSXP){
    for(int i = 0; i < LENGTH(x); i++){
      ra->set_digits(charxp2arrayint(STRING_ELT(x, i), ra->get_size()));
      LOGICAL(r)[i] = ra->validate_generic();
      INTEGER(l)[i] = ra->get_error();
      ra->clear();
    }

  }else if(TYPEOF(x) == REALSXP && is_bit64(x)){
    long long * q = (long long *)REAL(x);
    for(int i = 0; i < LENGTH(x); i++){
      ra->set_digits(bit642arrayint(q[i], ra->get_size(), ra->sizeRaValidate() ));
      LOGICAL(r)[i] = ra->validate_generic();
      INTEGER(l)[i] = ra->get_error();
      ra->clear();
    }

  }else if(TYPEOF(x) == REALSXP){
    double * q = REAL(x);
    for(int i = 0; i < LENGTH(x); i++){
      ra->set_digits(double2arrayint(q[i], ra->get_size(), ra->sizeRaValidate() ));
      LOGICAL(r)[i] = ra->validate_generic();
      INTEGER(l)[i] = ra->get_error();
      ra->clear();
    }
  }

  if(LOGICAL(log)[0] == 1){
    Rcpp::DataFrame NDF =
      Rcpp::DataFrame::create(Rcpp::Named("dado")=x,
                              Rcpp::Named("resultado")=r,
                              Rcpp::Named("erros")=l,
                              _["stringsAsFactors"] = false );
    UNPROTECT(2);
    return NDF.get__();
  }

  UNPROTECT(2);
  return r;
}

// [[Rcpp::export]]
void generate_digit(Rcpp::RObject x, SEXP type){
  const char *t = CHAR(STRING_ELT(type,0));

  // Utilização do ponteiro original (raw pointer)
  Ra *ra = factoryRa(t);

  if(x.sexp_type() == REALSXP && is_bit64(x.get__())){
    long long * q = (long long *)REAL(x.get__());
    for(int i = 0; i < LENGTH(x.get__()); i++){
      ra->set_digits(bit642arrayint(q[i], ra->get_size(), ra->sizeRaGenerateLastDigit() ));
      ra->generate_last_digit();
      q[i] = ra->int2bit64();
    }

  }else if(x.sexp_type() == REALSXP){
    double * q = REAL((x.get__()));
    for(int i = 0; i < LENGTH(x.get__()); i++){
      ra->set_digits(double2arrayint(q[i], ra->get_size(), ra->sizeRaGenerateLastDigit() ));
      ra->generate_last_digit();
      q[i] = ra->int2double();
    }
  }

  if(x.sexp_type() == STRSXP){
    for(int i = 0; i < LENGTH(x.get__()); i++){
      ra->set_digits(charxp2arrayint(STRING_ELT(x.get__(), i), ra->get_size()));
      ra->generate_last_digit();
      SEXP q = Rf_mkChar(ra->int2char());
      SET_STRING_ELT(x.get__(), i, q);
    }
  }
}
