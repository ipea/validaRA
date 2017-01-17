#include <Rcpp.h>
#include<vector>
#include<set>
using namespace Rcpp;

std::vector<int> remove_caracters_especiais(const std::string &x){
  std::vector<int> result;
  for(unsigned int i = 0; i < x.size(); i++){
    if(x[i] >= 48 && x[i] <= 57){
      result.push_back(x[i] - '0');
    }
  }
  return result;
}

static const int arr_primeiro_digito[] = {10,  9, 8, 7, 6, 5, 4, 3, 2, 0, 0};
static const int arr_segundo_digito[] =  {11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 0};
// [[Rcpp::export]]
LogicalVector valida_cpf(Rcpp::CharacterVector x){
  std::string cpf_string = Rcpp::as<std::string>(x);
  std::vector<int> y = remove_caracters_especiais(cpf_string);
  if(y.size() == 10) y.insert(y.begin(), 0);
  std::set<int> unicos(y.begin(), y.end());
  LogicalVector r(1);
  if(y.size() != 11 || unicos.size() == 1){
    r[0] = false;
    return r;
  }

  int primeiro_digito = 0;
  int segundo_digito = 0;
  for(unsigned int i = 0; i < y.size(); i++){
    primeiro_digito += y[i] * arr_primeiro_digito[i];
    segundo_digito += y[i] * arr_segundo_digito[i];
  }
  primeiro_digito = (primeiro_digito * 10) % 11;
  if(primeiro_digito == 10) primeiro_digito = 0;
  if(primeiro_digito != y[9]){
    r[0] = false;
    return r;
  }
  segundo_digito = (segundo_digito * 10) % 11;
  if(segundo_digito == 10) segundo_digito = 0;
  if(segundo_digito != y[10]){
    r[0] = false;
    return r;
  }
  r[0] = true;
  return r;
}

static const int primeiro_digito_cnpj[] = {5, 4, 3, 2, 9, 8, 7, 6, 5, 4, 3, 2, 0, 0};
static const int segundo_digito_cnpj[] =  {6, 5, 4, 3, 2, 9, 8, 7, 6, 5, 4, 3, 2, 0};
// [[Rcpp::export]]
LogicalVector valida_cnpj(Rcpp::CharacterVector x){
  std::string cpf_string = Rcpp::as<std::string>(x);
  std::vector<int> y = remove_caracters_especiais(cpf_string);
  if(y.size() == 10) y.insert(y.begin(), 0);
  LogicalVector r(1);
  if(y.size() != 11){
    r[0] = false;
    return r;
  }
  int primeiro_digito = 0;
  int segundo_digito = 0;
  for(unsigned int i = 0; i < y.size(); i++){
    primeiro_digito += y[i] * arr_primeiro_digito[i];
    segundo_digito += y[i] * arr_segundo_digito[i];
  }
  primeiro_digito = (primeiro_digito * 10) % 11;
  if(primeiro_digito == 10) primeiro_digito = 0;
  if(primeiro_digito != y[9]){
    r[0] = false;
    return r;
  }
  segundo_digito = (segundo_digito * 10) % 11;
  if(segundo_digito == 10) segundo_digito = 0;
  if(segundo_digito != y[10]){
    r[0] = false;
    return r;
  }
  r[0] = true;
  return r;
}

