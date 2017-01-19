#include <Rcpp.h>
#include<vector>
#include<set>
using namespace Rcpp;
static const double divisao[] = {1, 10, 100, 1000, 10000, 100000, 1000000,
                                       10000000, 100000000, 1000000000, 10000000000};
std::vector<int> numeric_to_vector(const Rcpp::NumericVector &x){
  std::vector<int> result;
  int r = 0;
  double number = x[0];
  for(int i = 10; i >= 0; i--){
      //std::cout << divisao[i] << std::endl;
      r = number/divisao[i];
      //std::cout << r << std::endl;
      number -= r * divisao[i];
      result.push_back(r);
  }
  return result;
}

static const double divisao_cpnj_d[] = {1, 10, 100, 1000, 10000, 100000, 1000000,
                                 10000000, 100000000, 1000000000, 10000000000};
static const double divisao_cpnj_a[] = {1, 10, 100, 1000};
std::vector<int> numeric_to_vector_cnpj(const Rcpp::NumericVector &x){
  std::vector<int> result;
  int r = 0;
  long double number = x[0];
  int new_number = 0;
  r = number/10000000000;
  new_number = r;
  number -= r*10000000000;
  for(int i = 3; i >= 0; i--){
    r = new_number/divisao[i];
    new_number -= r * divisao[i];
    result.push_back(r);
  }
  for(int i = 9; i >= 0; i--){
    r = number/divisao[i];
    number -= r * divisao[i];
    result.push_back(r);
  }
  return result;
}

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
LogicalVector valida_cpf(RObject t){
  bool caracter = is<CharacterVector>(t);
  GenericVector x(t);
  LogicalVector r(x.size());
  for(unsigned int j = 0; j < x.size(); j++){
    std::vector<int> y;

    if(caracter){
      std::string cpf_string = Rcpp::as<std::string>(x[j]);
      y = remove_caracters_especiais(cpf_string);
    }else{
      y = numeric_to_vector(x[j]);
    }
    std::set<int> unicos(y.begin(), y.end());

    if(y.size() != 11 || unicos.size() == 1){
      r[j] = false;
      continue;
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
      r[j] = false;
      continue;
    }
    segundo_digito = (segundo_digito * 10) % 11;
    if(segundo_digito == 10) segundo_digito = 0;
    if(segundo_digito != y[10]){
      r[j] = false;
      continue;
    }
    r[j] = true;
  }
  return r;
}
// [[Rcpp::export]]
Rcpp::DataFrame valida_cpf_log(RObject t){
  bool caracter = is<CharacterVector>(t);
  GenericVector x(t);
  LogicalVector r(x.size());
  Rcpp::CharacterVector log(x.size());
  for(unsigned int j = 0; j < x.size(); j++){
    std::vector<int> y;
    if(caracter){
      std::string cpf_string = Rcpp::as<std::string>(x[j]);
      y = remove_caracters_especiais(cpf_string);
    }else{
      y = numeric_to_vector(x[j]);
    }
    std::set<int> unicos(y.begin(), y.end());

    if(y.size() != 11){
      r[j] = false;
      log[j] = "Numero de caracters invalido";
      continue;
    }
    if(unicos.size() == 1){
      r[j] = false;
      log[j] = "Caracters iguais";
      continue;
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
      r[j] = false;
      log[j] = "Primero digito errado";
      continue;
    }
    segundo_digito = (segundo_digito * 10) % 11;
    if(segundo_digito == 10) segundo_digito = 0;
    if(segundo_digito != y[10]){
      r[j] = false;
      log[j] = "segundo digito errado";
      continue;
    }
    r[j] = true;
  }
  Rcpp::DataFrame resultado =   Rcpp::DataFrame::create(Rcpp::Named("dado")=t,
                          Rcpp::Named("resultado")=r,
                          Rcpp::Named("erros")=log);

  return resultado;
}

static const int primeiro_digito_cnpj[] = {5, 4, 3, 2, 9, 8, 7, 6, 5, 4, 3, 2, 0, 0};
static const int segundo_digito_cnpj[] =  {6, 5, 4, 3, 2, 9, 8, 7, 6, 5, 4, 3, 2, 0};
// [[Rcpp::export]]
LogicalVector valida_cnpj(RObject t){
  bool caracter = is<CharacterVector>(t);
  GenericVector x(t);
  LogicalVector r(x.size());
  for(unsigned int j = 0; j < x.size(); j++){
    std::vector<int> y;

    if(caracter){
      std::string cpf_string = Rcpp::as<std::string>(x[j]);
      y = remove_caracters_especiais(cpf_string);
    }else{
      y = numeric_to_vector_cnpj(x[j]);
    }
    std::set<int> unicos(y.begin(), y.end());

    if(y.size() != 14 || unicos.size() == 1){
      r[j] = false;
      continue;
    }

    int primeiro_digito = 0;
    int segundo_digito = 0;
    for(unsigned int i = 0; i < y.size(); i++){
      primeiro_digito += y[i] * primeiro_digito_cnpj[i];
      segundo_digito += y[i] * segundo_digito_cnpj[i];
    }
    primeiro_digito = (primeiro_digito * 10) % 11;
    if(primeiro_digito == 10) primeiro_digito = 0;
    if(primeiro_digito != y[12]){
      r[j] = false;
      continue;
    }
    segundo_digito = (segundo_digito * 10) % 11;
    if(segundo_digito == 10) segundo_digito = 0;
    if(segundo_digito != y[13]){
      r[j] = false;
      continue;
    }
    r[j] = true;
  }
  return r;
}

static const int digito_pis[] = {3, 2, 9, 8, 7, 6, 5, 4, 3, 2, 0};
// [[Rcpp::export]]
LogicalVector valida_pis(RObject t){
  bool caracter = is<CharacterVector>(t);
  GenericVector x(t);
  LogicalVector r(x.size());
  for(unsigned int j = 0; j < x.size(); j++){
    std::vector<int> y;

    if(caracter){
      std::string cpf_string = Rcpp::as<std::string>(x[j]);
      y = remove_caracters_especiais(cpf_string);
    }else{
      y = numeric_to_vector(x[j]);
    }
    std::set<int> unicos(y.begin(), y.end());

    if(y.size() != 11 || unicos.size() == 1){
      r[j] = false;
      continue;
    }

    int primeiro_digito = 0;
    for(unsigned int i = 0; i < y.size(); i++){
      primeiro_digito += y[i] * digito_pis[i];
    }
    primeiro_digito = (primeiro_digito * 10) % 11;
    if(primeiro_digito == 10) primeiro_digito = 0;
    if(primeiro_digito != y[10]){
      r[j] = false;
      continue;
    }
    r[j] = true;
  }
  return r;
}

