#include "boost.h"
#include <Rcpp.h>
#include <Rdefines.h>
#include <RInternals.h>
#include <math.h>

bool is_bit64(SEXP x){
  SEXP klass = Rf_getAttrib(x, R_ClassSymbol);
  if(TYPEOF(klass) == CHARSXP || TYPEOF(klass) == STRSXP){
    if(strcmp(CHAR(STRING_ELT(klass,0)),"integer64") == 0){
      return true;
    }
    return false;
  }
  return false;

}

int * charxp2arrayint(SEXP x, int *size){
  if(TYPEOF(x) != CHARSXP) std::cout << "Não é um charxt";
  const char *t = CHAR(x);
  int t_vec = 0;
  int len_v = (strlen(t) > 11) ? strlen(t) : 11;
  int *v = (int *)malloc(sizeof(int) * len_v) ;
  if(v == NULL){
    std::cout << "Não alocado";
    exit(1);
  }

  //std::cout << "o texto: " << t << std::endl;
  for(unsigned int j = 0; j < strlen(t); j++){
    try{
      //std::cout << boost::lexical_cast<int>(t[j]) << std::endl;
      v[t_vec] = boost::lexical_cast<int>(t[j]);
      t_vec++;
    }catch(...){
      continue;
    }
  }
  *(size) = t_vec;
  return v;
}

int * bit642arrayint(long long *t, int *size, int numbers_needed, int size_vec = 11){
  int t_vec = 0;
  int *v = (int *)malloc(sizeof(int) * size_vec);
  //std::cout << " " << t <<  std::endl;
  for(int j = numbers_needed ; j >= 0; j--){
    double base = powl(10,j);
    int n = *(t)/base;
    //std::cout << n << " " << t << " " << base << std::endl;
    *(t) -= (n*base);
    v[t_vec] = n;
    t_vec++;
  }
  *(size) = t_vec;
  return v;
}

int * bit642arrayint(long long t, int *size, int numbers_needed, int size_vec = 11){
  int t_vec = 0;
  int *v = (int *)malloc(sizeof(int) * size_vec);
  //std::cout << " " << t <<  std::endl;
  for(int j = numbers_needed ; j >= 0; j--){
    double base = powl(10,j);
    int n = t/base;
    //std::cout << n << " " << t << " " << base << std::endl;
    t -= (n*base);
    v[t_vec] = n;
    t_vec++;
  }
  *(size) = t_vec;
  return v;
}

int * double2arrayint(double *t, int *size, int numbers_needed, int size_vec = 11){
  int t_vec = 0;
  int *v = (int *)malloc(sizeof(int) * size_vec);
  //std::cout << " " << t <<  std::endl;
  for(int j = numbers_needed ; j >= 0; j--){
    double base = powl(10,j);
    int n = *(t)/base;
    //std::cout << n << " " << t << " " << base << std::endl;
    *(t) -= (n*base);
    v[t_vec] = n;
    t_vec++;
  }
  *(size) = t_vec;
  return v;
}

int * double2arrayint(double t, int *size, int numbers_needed, int size_vec = 11){
  int t_vec = 0;
  int *v = (int *)malloc(sizeof(int) * size_vec);
  //std::cout << " " << t <<  std::endl;
  for(int j = numbers_needed ; j >= 0; j--){
    double base = powl(10,j);
    int n = t/base;
    //std::cout << n << " " << t << " " << base << std::endl;
    t -= (n*base);
    v[t_vec] = n;
    t_vec++;
  }
  *(size) = t_vec;
  return v;
}

