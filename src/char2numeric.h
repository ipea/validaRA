#include "boost.h"
#include <Rcpp.h>
#include<vector>
#include<set>
#include <algorithm>
#include <string>
#include <R.h>
#include <Rdefines.h>
#include <RInternals.h>


void convert_char2arrayint(SEXP x, int ** v, int *size){
  *v = (int *) malloc(sizeof(int) * LENGTH(x));
  if(!Rf_isString(x)) std::cout << "Não é um charxt";

  for(int i = 0; i < LENGTH(x); i++){
    const char *t = CHAR(STRING_ELT(x, i));
    for(unsigned int j = 0; j < strlen(t); j++){
      try{
        int n = boost::lexical_cast<int>(t[j]);
        *v[i] = n;
        *(size)++;
      }catch(...){
        continue;
      }
    }
  }
}


