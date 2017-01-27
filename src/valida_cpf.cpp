#include <Rcpp.h>
#include<vector>
#include<set>
#include <algorithm>
#define tpis_cpf 11
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
    if(cpf_string.size() == 0){
      r[j] = false;
      continue;
    }
    int cpf_limpo[tpis_cpf];
    int t = 0;
    for(unsigned int i = 0; i < cpf_string.size(); i++){
      if(cpf_string[i] >= 48 && cpf_string[i] <= 57){
        cpf_limpo[t%tpis_cpf] = cpf_string[i] - '0';
        t++;
      }
    }
    if(t < tpis_cpf){
      for(int i = 10, q = (t-1); q >= 0; i--, q--){
        cpf_limpo[i] = cpf_limpo[q];
      }
      std::fill_n(cpf_limpo, tpis_cpf - t, 0);
      t = tpis_cpf;
    }
    std::set<int> unicos(cpf_limpo, cpf_limpo+tpis_cpf);

    if(t > tpis_cpf || unicos.size() == 1){
      r[j] = false;
      continue;
    }

    int primeiro_digito = 0;
    int segundo_digito = 0;
    for(unsigned int i = 0; i < tpis_cpf; i++){
      primeiro_digito += cpf_limpo[i] * arr_primeiro_digito[i];
      segundo_digito += cpf_limpo[i] * arr_segundo_digito[i];
    }
    primeiro_digito = (primeiro_digito * 10) % tpis_cpf;
    if(primeiro_digito == 10) primeiro_digito = 0;
    if(primeiro_digito != cpf_limpo[9]){
      r[j] = false;
      continue;
    }
    segundo_digito = (segundo_digito * 10) % tpis_cpf;
    if(segundo_digito == 10) segundo_digito = 0;
    if(segundo_digito != cpf_limpo[10]){
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
    if(cpf_string.size() == 0){
      log[j] = "Sem characters";
      r[j] = false;
      continue;
    }
    int cpf_limpo[tpis_cpf];
    int t = 0;
    for(unsigned int i = 0; i < cpf_string.size(); i++){
      if(cpf_string[i] >= 48 && cpf_string[i] <= 57){
        cpf_limpo[t%tpis_cpf] = cpf_string[i] - '0';
        t++;
      }
    }
    if(t > tpis_cpf){
      r[j] = false;
      log[j] = "Numero de caracters invalido";
      continue;
    }
    if(t < tpis_cpf){
      for(int i = 10, q = (t-1); q >= 0; i--, q--){
        cpf_limpo[i] = cpf_limpo[q];
      }
      std::fill_n(cpf_limpo, tpis_cpf - t, 0);
      t = tpis_cpf;
    }

    std::set<int> unicos(cpf_limpo, cpf_limpo+tpis_cpf);

    if(unicos.size() == 1){
      r[j] = false;
      if(cpf_limpo[0] == 0){
        log[j] = "Zeros";
      }else{
        log[j] = "Caracters iguais";
      }
      continue;
    }



    int primeiro_digito = 0;
    int segundo_digito = 0;
    for(unsigned int i = 0; i < tpis_cpf; i++){
      primeiro_digito += cpf_limpo[i] * arr_primeiro_digito[i];
      segundo_digito += cpf_limpo[i] * arr_segundo_digito[i];
    }
    primeiro_digito = (primeiro_digito * 10) % tpis_cpf;
    if(primeiro_digito == 10) primeiro_digito = 0;
    if(primeiro_digito != cpf_limpo[9]){
      r[j] = false;
      log[j] = "Primero digito errado";
      continue;
    }
    segundo_digito = (segundo_digito * 10) % tpis_cpf;
    if(segundo_digito == 10) segundo_digito = 0;
    if(segundo_digito != cpf_limpo[10]){
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


