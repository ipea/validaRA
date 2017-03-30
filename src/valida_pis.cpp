#include "boost.h"
#include "convert2int.h"
#include <Rcpp.h>
#include<vector>
#include<set>
#include <algorithm>
#include <string>
#include <R.h>
#include <Rdefines.h>
#include <RInternals.h>


#define tpis_cpf 11
using namespace Rcpp;
static const int digito_pis[] = {3, 2, 9, 8, 7, 6, 5, 4, 3, 2, 0};
class Pis{
private:
  int * digits;
  int error;
  unsigned int size;
public:
  Pis(){ this->size = 0; };
  Pis(int* digits_value){ set_digits(digits_value); };
  int generate_last_digit();
  bool validate();
  void set_digits(int *p);
  int * get_size(){ int * q = (int *)&size; return q;}
  int * get_digits(){return digits;}
  bool has_error(){ return error; }
  void print_pis();
  void clear(){ this->size = 0;}
  char *  int2char();
  long long int2bit64();
  double int2double();
  void push(int n){
    digits[this->size] = n;
    this->size++;
  }
};

long long Pis::int2bit64(){
  long long num = 0;
  //std::cout << "Size " << this->size << std::endl;
  unsigned int temp_size = this->size - 1;
  for(unsigned int i = 0; i <= temp_size; i++){
    num += (pow(10,i) * digits[temp_size - i]);
    //std::cout << num << std::endl;
  }
  return num;
}

double Pis::int2double(){
  double num = 0;
  //std::cout << "Size " << this->size << std::endl;
  unsigned int temp_size = this->size - 1;
  for(unsigned int i = 0; i <= temp_size; i++){
    num += (pow(10,i) * digits[temp_size - i]);
    //std::cout << num << std::endl;
  }
  return num;
}

char *  Pis::int2char(){
  char * c = new char[size + 1];
  for(unsigned int i = 0; i < size; i++){
    try{
      c[i] = boost::lexical_cast<char>(digits[i]) ;
    }catch(...){
      std::cout << "Erro aqui na funcao" << std::endl;
      continue;
    }
  }
  c[size] = '\0';
  return c;
}
void Pis::print_pis(){
  for(int i = 0; i < 11; i++){
    std::cout << digits[i] << "," ;
  }
  std::cout << std::endl;
}
void Pis::set_digits(int *p){
  digits = p;
}

int Pis::generate_last_digit(){
  int result = 0;
  for(int i = 0; i < tpis_cpf; i++){
    result += digits[i] * digito_pis[i];
  }
  result = (result * 10) % tpis_cpf;
  if(result == 10) result = 0;
  digits[10] = result;
  size++;
  return result;
}

bool Pis::validate(){
  bool r = false;
  int result = 0;
  if(size < tpis_cpf){
    std::fill_n(digits + size, tpis_cpf - size, 0);
  }
  for(int i = 0; i < tpis_cpf; i++){
    result += digits[i] * digito_pis[i];
  }
  result = (result * 10) % tpis_cpf;
  if(result == 10) result = 0;
  if(result == digits[10]){
    r = true;
  }else{
    error = 1;
  }
  return r;
}

// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::export]]
void test(){
  int digito[] = {1,2,0,7,4,3,6,2,7,8,3};
  int digito2[] = {1,2,0,7,4,3,6,2,7,8,0};
  Pis p;
  for(int i = 0; i < 11; i++){
    p.push(digito[i]);
  }

  p.print_pis();
  std::cout << p.int2char() << std::endl;
  std::cout << p.validate() << std::endl;
  digito[3] = 8;

  Pis p2(digito);
  std::cout << p2.validate() << std::endl;
  Pis p3(digito2);
  std::cout << p3.generate_last_digit() << std::endl;
  std::string s = "18609070662";

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
  if(x.sexp_type() == STRSXP){
    Pis pis;
    for(int i = 0; i < LENGTH(x.get__()); i++){
      pis.set_digits(charxp2arrayint(STRING_ELT(x.get__(), i),pis.get_size()));
      LOGICAL(r)[i] = pis.validate();
    }

  }else if(x.sexp_type() == REALSXP){
    Pis pis;
    for(int i = 0; i < LENGTH(x.get__()); i++){
      double t = REAL(x.get__())[i];
      for(int j = (tpis_cpf-1); j >= 0; j--){
        double base = powl(10,j);
        int n = t/base;
        //std::cout << n << " " << t << " " << base << std::endl;
        t -= (n*base);
        pis.push(n);
      }
      //pis.print_pis();
      LOGICAL(r)[i] = pis.validate();
      pis.clear();
    }

  }
  UNPROTECT(1);
  return r;
}
// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::export]]
SEXP valida_pis_2(Rcpp::RObject x){
  bool  r = true;
  if(x.sexp_type() == STRSXP){
    Rcpp::CharacterVector t(x);
    Pis pis;
    for(unsigned int j = 0; j < t.size(); j++){
      std::string cpf_string = Rcpp::as<std::string>(t[j]);
      for(unsigned int i = 0; i < cpf_string.size(); i++ ){
        if(std::isdigit(cpf_string[i])){

          pis.push((cpf_string[i]));
        }
      }
    }
  }

  return Rcpp::wrap(r);
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

