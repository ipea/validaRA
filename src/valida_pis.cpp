#include <Rcpp.h>
#include<vector>
#include<set>
#include <algorithm>
#define tpis_cpf 11
using namespace Rcpp;
static const int digito_pis[] = {3, 2, 9, 8, 7, 6, 5, 4, 3, 2, 0};
class Pis{
private:
  int * digits;
  int error;
  void set_digits(int *p);
public:
  Pis(int* digits_value){ set_digits(digits_value); };
  int generate_last_digit();
  bool validate();
  bool has_error(){ return error; }
  void print_pis();
};

void Pis::print_pis(){
  for(int i = 0; i < 11; i++){
    std::cout << digits[i] << "," ;
  }
  std::cout << std::endl;
}
void Pis::set_digits(int *p){
  int size = sizeof(p)/sizeof(int);
  std::cout << "Size " << size << std::endl;
  if(size == 11){
    digits = p;
  }else{
    digits = new int[11];
    print_pis();
    std::copy(p, p+size, digits);
  }

}

int Pis::generate_last_digit(){
  int result = 0;
  for(int i = 0; i < tpis_cpf; i++){
    result += digits[i] * digito_pis[i];
  }
  result = (result * 10) % tpis_cpf;
  if(result == 10) result = 0;
  digits[10] = result;
  return result;
}

bool Pis::validate(){
  bool r = false;
  int result = 0;
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
  Pis p(digito);
  p.print_pis();
  std::cout << p.validate() << std::endl;
  digito[3] = 8;
  Pis p2(digito);
  std::cout << p2.validate() << std::endl;
  Pis p3(digito2);
  std::cout << p3.generate_last_digit() << std::endl;
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

