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
  LogicalVector r(x.size());
  for(unsigned int j = 0; j < x.size(); j++){
    std::string cpf_string = Rcpp::as<std::string>(x[j]);
    std::vector<int> y = remove_caracters_especiais(cpf_string);
    if(y.size() == 10) y.insert(y.begin(), 0);
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
Rcpp::DataFrame valida_cpf_log(Rcpp::CharacterVector x){
  LogicalVector r(x.size());
  Rcpp::CharacterVector log(x.size());
  for(unsigned int j = 0; j < x.size(); j++){
    std::string cpf_string = Rcpp::as<std::string>(x[j]);
    std::vector<int> y = remove_caracters_especiais(cpf_string);
    if(y.size() == 10) y.insert(y.begin(), 0);
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
  Rcpp::DataFrame resultado =   Rcpp::DataFrame::create(Rcpp::Named("dado")=x,
                          Rcpp::Named("resultado")=r,
                          Rcpp::Named("erros")=log);

  return resultado;
}

static const int primeiro_digito_cnpj[] = {5, 4, 3, 2, 9, 8, 7, 6, 5, 4, 3, 2, 0, 0};
static const int segundo_digito_cnpj[] =  {6, 5, 4, 3, 2, 9, 8, 7, 6, 5, 4, 3, 2, 0};
// [[Rcpp::export]]
LogicalVector valida_cnpj(Rcpp::CharacterVector x){
  LogicalVector r(x.size());
  for(unsigned int j = 0; j < x.size(); j++){
    std::string cpf_string = Rcpp::as<std::string>(x[j]);
    std::vector<int> y = remove_caracters_especiais(cpf_string);
    if(y.size() == 10) y.insert(y.begin(), 0);
    std::set<int> unicos(y.begin(), y.end());

    if(y.size() != 14 || unicos.size() == 1){
      //std::cout << "Erro no tamanho ou no digitos unicos" << std::endl;
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
      //std::cout << "Erro no prmeiro digito" << std::endl;
      r[j] = false;
      continue;
    }
    segundo_digito = (segundo_digito * 10) % 11;
    if(segundo_digito == 10) segundo_digito = 0;
    if(segundo_digito != y[13]){
      //std::cout << "Erro no segundo digito" << std::endl;
      //std::cout << "Erro no segundo digito" << std::endl;
      r[j] = false;
      continue;
    }
    r[j] = true;
  }
  return r;
}

static const int digito_pis[] = {3, 2, 9, 8, 7, 6, 5, 4, 3, 2, 0};
// [[Rcpp::export]]
LogicalVector valida_pis(Rcpp::CharacterVector x){
  LogicalVector r(x.size());
  for(unsigned int j = 0; j < x.size(); j++){
    std::string cpf_string = Rcpp::as<std::string>(x[j]);
    std::vector<int> y = remove_caracters_especiais(cpf_string);
    if(y.size() == 10) y.insert(y.begin(), 0);
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

