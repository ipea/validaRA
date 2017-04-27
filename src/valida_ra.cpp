#include "boost.h"
#include "convert2int.h"
#include "cpf.h"
#include "Pis.h"
#include "utils.h"
#include <Rcpp.h>
#include <algorithm>
#include <string>
#include <R.h>
#include <Rdefines.h>
#include <RInternals.h>


// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::export]]
SEXP valida_ra(SEXP x, SEXP type, SEXP log){
  SEXP r = PROTECT(Rf_allocVector(LGLSXP, LENGTH(x)));
  SEXP l = PROTECT(Rf_allocVector(INTSXP, LENGTH(x)));
  const char *t = CHAR(STRING_ELT(type,0));
  Ra *ra = factoryRa(t);
  //std::cout << "Is factor: " << is_factor(x) << std::endl;
  if(is_factor(x)){
    for(int i = 0; i < LENGTH(x); i++){
      ra->set_digits(charxp2arrayint(STRING_ELT(x, i),ra->get_size()));
      LOGICAL(r)[i] = ra->validate();
      INTEGER(l)[i] = ra->get_error();

    }

  }else if(TYPEOF(x) == STRSXP){
    for(int i = 0; i < LENGTH(x); i++){
      ra->set_digits(charxp2arrayint(STRING_ELT(x, i),ra->get_size()));
      LOGICAL(r)[i] = ra->validate();
      INTEGER(l)[i] = ra->get_error();

    }

  }else if(TYPEOF(x) == REALSXP && is_bit64(x)){
    long long * q = (long long *)REAL(x);
    for(int i = 0; i < LENGTH(x); i++){
      ra->set_digits(bit642arrayint(q[i], ra->get_size(),ra->sizeRaValidate() ));
      LOGICAL(r)[i] = ra->validate();
      INTEGER(l)[i] = ra->get_error();
    }
  }else if(TYPEOF(x) == REALSXP){
    double * q = REAL(x);
    for(int i = 0; i < LENGTH(x); i++){
      ra->set_digits(double2arrayint(q[i], ra->get_size(),ra->sizeRaValidate() ));
      LOGICAL(r)[i] = ra->validate();
      INTEGER(l)[i] = ra->get_error();
    }

  }
 // std::cout << "error " <<  ra->get_error() << std::endl;
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


