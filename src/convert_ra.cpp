#include <Rcpp.h>
#include<vector>
#include<set>
#include <algorithm>
#define tpis_cpf 11
using namespace Rcpp;

// [[Rcpp::export]]
SEXP converter_ra_(Rcpp::RObject x){
  Environment myEnv = Environment::global_env();
  Function int64 = myEnv.find("as.integer64");
  if(x.sexp_type() == STRSXP){
    //std::cout << "passou aqui" << std::endl;
    CharacterVector y(x);
    std::vector<std::string> resultado(y.size());
    for(unsigned int i = 0; i < y.size(); i++){
      std::string ra_string = Rcpp::as<std::string>(y[i]);
      //std::cout << ra_string << std::endl;
      for(unsigned int j = 0; j < ra_string.size(); j++){
        if(!std::isdigit(ra_string[j])){
          ra_string.erase(j, 1);
        }
      }
      resultado[i] = ra_string;
      //std::cout << t << std::endl;

    }

    return int64(Rcpp::Named("x", resultado));
  }
  return int64(Rcpp::Named("x", x));
}

