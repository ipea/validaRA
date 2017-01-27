#include <Rcpp.h>
#include<vector>
#include<set>
#include <algorithm>
#define tcnpj 14
using namespace Rcpp;

static const int primeiro_digito_cnpj[] = {5, 4, 3, 2, 9, 8, 7, 6, 5, 4, 3, 2, 0, 0};
static const int segundo_digito_cnpj[] =  {6, 5, 4, 3, 2, 9, 8, 7, 6, 5, 4, 3, 2, 0};
// [[Rcpp::export]]
LogicalVector valida_cnpj(Rcpp::CharacterVector x){
  LogicalVector r(x.size());
  for(unsigned int j = 0; j < x.size(); j++){
    std::string cnpj_string = Rcpp::as<std::string>(x[j]);
    if(cnpj_string.size() == 0){
      r[j] = false;
      continue;
    }
    int cnpj_limpo[tcnpj];
    int t = 0;
    for(unsigned int i = 0; i < cnpj_string.size(); i++){
      if(cnpj_string[i] >= 48 && cnpj_string[i] <= 57){
        cnpj_limpo[t%tcnpj] = cnpj_string[i] - '0';
        t++;
      }
    }
    if(t < tcnpj){
      for(int i = 10, q = (t-1); q >= 0; i--, q--){
        cnpj_limpo[i] = cnpj_limpo[q];
      }
      std::fill_n(cnpj_limpo, tcnpj - t, 0);
      t = tcnpj;
    }
    std::set<int> unicos(cnpj_limpo, cnpj_limpo+tcnpj);
    if(t > tcnpj || unicos.size() == 1){
      r[j] = false;
      continue;
    }
    int primeiro_digito = 0;
    int segundo_digito = 0;
    for(unsigned int i = 0; i < tcnpj; i++){
      primeiro_digito += cnpj_limpo[i] * primeiro_digito_cnpj[i];
      segundo_digito += cnpj_limpo[i] * segundo_digito_cnpj[i];
    }
    primeiro_digito = (primeiro_digito * 10) % 11;
    if(primeiro_digito == 10) primeiro_digito = 0;
    if(primeiro_digito != cnpj_limpo[12]){
      r[j] = false;
      continue;
    }
    segundo_digito = (segundo_digito * 10) % 11;
    if(segundo_digito == 10) segundo_digito = 0;
    if(segundo_digito != cnpj_limpo[13]){
      r[j] = false;
      continue;
    }
    r[j] = true;
  }
  return r;
}

// [[Rcpp::export]]
Rcpp::DataFrame valida_cnpj_log(Rcpp::CharacterVector x){
  LogicalVector r(x.size());
  Rcpp::CharacterVector log(x.size());
  for(unsigned int j = 0; j < x.size(); j++){
    std::string cnpj_string = Rcpp::as<std::string>(x[j]);
    if(cnpj_string.size() == 0){
      r[j] = false;
      log[j] = "Sem characters";
      continue;
    }
    int cnpj_limpo[tcnpj];
    int t = 0;
    for(unsigned int i = 0; i < cnpj_string.size(); i++){
      if(cnpj_string[i] >= 48 && cnpj_string[i] <= 57){
        cnpj_limpo[t%tcnpj] = cnpj_string[i] - '0';
        t++;
      }
    }
    if(t > tcnpj){
      r[j] = false;
      log[j] = "Numero de caracters invalido";
      continue;
    }
    if(t < tcnpj){
      for(int i = 10, q = (t-1); q >= 0; i--, q--){
        cnpj_limpo[i] = cnpj_limpo[q];
      }
      std::fill_n(cnpj_limpo, tcnpj - t, 0);
      t = tcnpj;
    }
    std::set<int> unicos(cnpj_limpo, cnpj_limpo+tcnpj);
    if(unicos.size() == 1){
      r[j] = false;
      if(cnpj_limpo[0] == 0){
        log[j] = "Zeros";
      }else{
        log[j] = "Caracters iguais";
      }
      continue;
    }

    int primeiro_digito = 0;
    int segundo_digito = 0;
    for(unsigned int i = 0; i < tcnpj; i++){
      primeiro_digito += cnpj_limpo[i] * primeiro_digito_cnpj[i];
      segundo_digito += cnpj_limpo[i] * segundo_digito_cnpj[i];
    }
    primeiro_digito = (primeiro_digito * 10) % 11;
    if(primeiro_digito == 10) primeiro_digito = 0;
    if(primeiro_digito != cnpj_limpo[12]){
      r[j] = false;
      log[j] = "Primero digito errado";
      continue;
    }
    segundo_digito = (segundo_digito * 10) % 11;
    if(segundo_digito == 10) segundo_digito = 0;
    if(segundo_digito != cnpj_limpo[13]){
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
