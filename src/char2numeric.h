#include "boost.h"
#include <Rcpp.h>
#include <Rdefines.h>
#include <RInternals.h>
#include <math.h>

int * charxp2arrayint(SEXP x, int *size){
  if(TYPEOF(x) != CHARSXP) std::cout << "Não é um charxt";
  const char *t = CHAR(x);
  int len_v = (strlen(t) > 11) ? strlen(t) : 11;
  int *v = (int *)malloc(sizeof(int) * len_v) ;
  if(v == NULL){
    std::cout << "Não alocado";
    exit(1);
  }

  //std::cout << "o texto: " << t << std::endl;
  for(unsigned int j = 0; j < strlen(t); j++){
    try{
      v[j] = boost::lexical_cast<int>(t[j]);;
      *(size) = *(size) + 1;
    }catch(...){
      continue;
    }
  }
  return v;
}


