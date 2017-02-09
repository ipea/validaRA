#include <Rcpp.h>
#include<vector>
#include<set>
#include <algorithm>
#define tpis_cpf 11
using namespace Rcpp;
static const double base_m[] = {1,
                                10,
                                100,
                                1000,
                                10000,
                                100000,
                                1000000,
                                10000000,
                                100000000,
                                1000000000,
                                10000000000};

// [[Rcpp::export]]
NumericVector convert_ra(Rcpp::RObject x){
  if(x.sexp_type() == STRSXP){
    //std::cout << "passou aqui" << std::endl;
    CharacterVector y(x);
    NumericVector resultado(y.size());
    for(unsigned int i = 0; i < y.size(); i++){
      std::string ra_string = Rcpp::as<std::string>(y[i]);
      //std::cout << ra_string << std::endl;
      double t = 0;
      int base = 0;
      for(int j = (int)ra_string.size(); j >= 0; j--){
        if(ra_string[j] >= 48 && ra_string[j] <= 57){
          t += ((ra_string[j] - '0') * base_m[base]);
          base++;
        }
      }
      //std::cout << t << std::endl;
      resultado[i] = t;
    }
    return resultado;
  }else if(x.sexp_type() == REALSXP){
    return NumericVector(x);
  }
  return NumericVector();
}

