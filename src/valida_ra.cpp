#include "boost.h"
#include "convert2int.h"
#include <Rcpp.h>
#include <algorithm>
#include "utils.h"
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

// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::export]]
void generate_digit_pis(Rcpp::RObject x){
  Pis pis;
  if(x.sexp_type() == REALSXP && is_bit64(x.get__())){
    long long * q = (long long *)REAL(x.get__());
    //std::cout << " " << LENGTH(x.get__())  << std::endl;
    for(int i = 0; i < LENGTH(x.get__()); i++){
      pis.set_digits(bit642arrayint(&q[i], pis.get_size(),pis.sizeRaGenerateLastDigit() ));
      pis.generate_last_digit();
      q[i] = pis.int2bit64();
      //pis.print_pis();
    }
    //std::cout << "falskjflakjfslaksjflakjfslaksfjlakf" << std::endl;
  }else if(x.sexp_type() == REALSXP){
    double * q = REAL((x.get__()));
    for(int i = 0; i < LENGTH(x.get__()); i++){
      pis.set_digits(double2arrayint(&q[i], pis.get_size(),pis.sizeRaGenerateLastDigit() ));
      pis.generate_last_digit();
      q[i] = pis.int2double();
      //pis.print_pis();
    }
  }

  if(x.sexp_type() == STRSXP){
    for(int i = 0; i < LENGTH(x.get__()); i++){
      pis.set_digits(charxp2arrayint(STRING_ELT(x.get__(), i),pis.get_size()));
      pis.generate_last_digit();
      //pis.print_pis();
      SEXP q = Rf_mkChar(pis.int2char());
      //std::cout << TYPEOF(q) << std::endl;
      SET_STRING_ELT(x.get__(), i, q);

    }
  }
}
