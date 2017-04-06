#include "boost.h"
#include "convert2int.h"
#include "Pis.h"
#include <Rcpp.h>
#include<vector>
#include<set>
#include <algorithm>
#include <string>
#include <R.h>
#include <Rdefines.h>
#include <RInternals.h>




// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::export]]
void test(){
  int digito[] = {1,2,0,7,4,3,6,2,7,8,3};
  int digito2[] = {1,2,0,7,4,3,6,2,7,8,0};
  Pis p;
  for(int i = 0; i < 11; i++){
    p.push(digito[i]);
  }

  std::cout << p.int2char() << std::endl;
  //std::cout << p.validate() << std::endl;
  //digito[3] = 8;

  Pis p2(digito, 11);
  std::cout << p2.validate() << std::endl;
  Pis p3(digito2,11);
  std::cout << p3 << std::endl;
  std::cout << p3.generate_last_digit() << std::endl;



}


// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::export]]
void generate_digit_pis(Rcpp::RObject x){
  Pis pis;
  if(x.sexp_type() == REALSXP && is_bit64(x.get__())){
    long long * q = (long long *)REAL(x.get__());
    //std::cout << " " << LENGTH(x.get__())  << std::endl;
    for(int i = 0; i < LENGTH(x.get__()); i++){
      int porra = 9;
      pis.set_digits(bit642arrayint(&q[i], pis.get_size(),&porra ));
      pis.generate_last_digit();
      q[i] = pis.int2bit64();
      //pis.print_pis();
    }
    //std::cout << "falskjflakjfslaksjflakjfslaksfjlakf" << std::endl;
  }else if(x.sexp_type() == REALSXP){
    double * q = REAL((x.get__()));
    for(int i = 0; i < LENGTH(x.get__()); i++){
      int porra = 9;
      pis.set_digits(double2arrayint(&q[i], pis.get_size(),&porra ));
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
// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::export]]
SEXP valida_pis_3(Rcpp::RObject x){
  SEXP r = PROTECT(Rf_allocVector(LGLSXP, LENGTH(x.get__())));
  Pis pis;
  if(x.sexp_type() == STRSXP){
    for(int i = 0; i < LENGTH(x.get__()); i++){
      pis.set_digits(charxp2arrayint(STRING_ELT(x.get__(), i),pis.get_size()));
      LOGICAL(r)[i] = pis.validate();
    }

  }else if(x.sexp_type() == REALSXP && is_bit64(x.get__())){
    long long * q = (long long *)REAL(x.get__());
    for(int i = 0; i < LENGTH(x.get__()); i++){
      int tamanho = 10;
      pis.set_digits(bit642arrayint(q[i], pis.get_size(),&tamanho ));
      LOGICAL(r)[i] = pis.validate();
    }
  }else if(x.sexp_type() == REALSXP){
    double * q = REAL(x.get__());
    for(int i = 0; i < LENGTH(x.get__()); i++){
      int tamanho = 10;
      pis.set_digits(double2arrayint(q[i], pis.get_size(),&tamanho ));
      LOGICAL(r)[i] = pis.validate();
    }

  }
  UNPROTECT(1);
  return r;
}

// [[Rcpp::export]]
LogicalVector valida_pis(Rcpp::CharacterVector x){
  LogicalVector r(x.size());
  for(unsigned int j = 0; j < x.size(); j++){
    std::string cpf_string = Rcpp::as<std::string>(x[j]);
    if(cpf_string.size() == 0){
      r[j] = false;
      continue;
    }
    int pis_limpo[tpis_cpf];
    int t = 0;
    for(unsigned int i = 0; i < cpf_string.size(); i++){
      if(cpf_string[i] >= 48 && cpf_string[i] <= 57){
        pis_limpo[t%tpis_cpf] = cpf_string[i] - '0';
        t++;
      }
    }
    if(t < tpis_cpf){
      for(int i = 10, q = (t-1); q >= 0; i--, q--){
        pis_limpo[i] = pis_limpo[q];
      }
      std::fill_n(pis_limpo, tpis_cpf - t, 0);
      t = tpis_cpf;
    }
    std::set<int> unicos(pis_limpo, pis_limpo+tpis_cpf);

    if(t > tpis_cpf || unicos.size() == 1){
      r[j] = false;
      continue;
    }

    int primeiro_digito = 0;
    for(unsigned int i = 0; i < tpis_cpf; i++){
      primeiro_digito += pis_limpo[i] * digito_pis[i];
    }
    primeiro_digito = (primeiro_digito * 10) % tpis_cpf;
    if(primeiro_digito == 10) primeiro_digito = 0;
    if(primeiro_digito != pis_limpo[10]){
      r[j] = false;
      continue;
    }
    r[j] = true;
  }
  return r;
}
// [[Rcpp::export]]
Rcpp::DataFrame valida_pis_log(Rcpp::CharacterVector x){
  LogicalVector r(x.size());
  Rcpp::CharacterVector log(x.size());
  for(unsigned int j = 0; j < x.size(); j++){
    std::string cpf_string = Rcpp::as<std::string>(x[j]);
    if(cpf_string.size() == 0){
      log[j] = "Sem characters";
      r[j] = false;
      continue;
    }
    int pis_limpo[tpis_cpf];
    int t = 0;
    for(unsigned int i = 0; i < cpf_string.size(); i++){
      if(cpf_string[i] >= 48 && cpf_string[i] <= 57){
        pis_limpo[t%tpis_cpf] = cpf_string[i] - '0';
        t++;
      }
    }
    if(t < tpis_cpf){
      for(int i = 10, q = (t-1); q >= 0; i--, q--){
        pis_limpo[i] = pis_limpo[q];
      }
      std::fill_n(pis_limpo, tpis_cpf - t, 0);
      t = tpis_cpf;
    }
    if(t > tpis_cpf){
      r[j] = false;
      log[j] = "Numero de caracters invalido";
      continue;
    }
    std::set<int> unicos(pis_limpo, pis_limpo+tpis_cpf);

    if(unicos.size() == 1){
      r[j] = false;
      if(pis_limpo[0] == 0){
        log[j] = "Zeros";
      }else{
        log[j] = "Caracters iguais";
      }
      continue;
    }

    int primeiro_digito = 0;
    for(unsigned int i = 0; i < tpis_cpf; i++){
      primeiro_digito += pis_limpo[i] * digito_pis[i];
    }
    primeiro_digito = (primeiro_digito * 10) % tpis_cpf;
    if(primeiro_digito == 10) primeiro_digito = 0;
    if(primeiro_digito != pis_limpo[10]){
      r[j] = false;
      log[j] = "Primero digito errado";
      continue;
    }
    r[j] = true;
  }
  Rcpp::DataFrame resultado =   Rcpp::DataFrame::create(Rcpp::Named("dado")=x,
                                                        Rcpp::Named("resultado")=r,
                                                        Rcpp::Named("erros")=log,
                                                        Rcpp::Named("stringsAsFactors")=false);
  return resultado;
}

