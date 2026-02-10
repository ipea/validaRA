#include "convert2int.h"
#include <R.h>
#include <Rinternals.h>
#include <cstring>
#include <cmath>
#include <cctype>

// =========================================================
// CORRECAO DE CRASH (Heap Corruption):
// Definimos um tamanho minimo seguro (MIN_SAFE_SIZE = 16).
// Isso garante que mesmo se o usuario passar string vazia "" ou curta "123",
// o validador (que espera 11 ou 14 digitos) nao le memory fora do array.
// =========================================================

#define MIN_SAFE_SIZE 16

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

bool is_factor(SEXP x){
  SEXP klass = Rf_getAttrib(x, R_ClassSymbol);
  if(TYPEOF(klass) == CHARSXP || TYPEOF(klass) == STRSXP){
    if(strcmp(CHAR(STRING_ELT(klass,0)),"factor") == 0){
      return true;
    }
    return false;
  }
  return false;
}

// Converte string R (CHARSXP) para array de inteiros
int * charxp2arrayint(SEXP x, int *size){
  const char *t = CHAR(x);
  int len_t = std::strlen(t);

  // CORRECAO: Aloca o MAIOR entre o tamanho da string e 16.
  // Isso impede o crash se a string for menor que o esperado pelo validador.
  int alloc_size = (len_t > MIN_SAFE_SIZE) ? len_t + 1 : MIN_SAFE_SIZE;

  // Usa R_alloc para o R gerenciar a memoria (sem memory leaks)
  int *v = (int *) R_alloc(alloc_size, sizeof(int));

  // Zera a memoria para garantir que digitos faltantes sejam 0
  std::memset(v, 0, alloc_size * sizeof(int));

  int t_vec = 0;
  for(int j = 0; j < len_t; j++){
    if(isdigit(t[j])){
      v[t_vec] = t[j] - '0';
      t_vec++;
    }
  }
  *(size) = t_vec;
  return v;
}

// Versao Bit64 (ponteiro)
int * bit642arrayint(long long *t, int *size, int numbers_needed, int size_vec){
  int alloc_size = (numbers_needed + 2 > size_vec) ? (numbers_needed + 2) : size_vec;
  if (alloc_size < MIN_SAFE_SIZE) alloc_size = MIN_SAFE_SIZE; // Garante tamanho minimo

  int *v = (int *) R_alloc(alloc_size, sizeof(int));
  std::memset(v, 0, alloc_size * sizeof(int));

  int t_vec = 0;
  for(int j = numbers_needed ; j >= 0; j--){
    double base = std::pow(10, j);
    long long val = *t;
    int n = (int)(val / (long long)base);
    *t -= (n * (long long)base);
    v[t_vec] = n;
    t_vec++;
  }
  *(size) = t_vec;
  return v;
}

// Versao Bit64 (valor)
int * bit642arrayint(long long t, int *size, int numbers_needed, int size_vec){
  int alloc_size = (numbers_needed + 2 > size_vec) ? (numbers_needed + 2) : size_vec;
  if (alloc_size < MIN_SAFE_SIZE) alloc_size = MIN_SAFE_SIZE;

  int *v = (int *) R_alloc(alloc_size, sizeof(int));
  std::memset(v, 0, alloc_size * sizeof(int));

  int t_vec = 0;
  for(int j = numbers_needed ; j >= 0; j--){
    double base = std::pow(10, j);
    int n = (int)(t / (long long)base);
    t -= (n * (long long)base);
    v[t_vec] = n;
    t_vec++;
  }
  *(size) = t_vec;
  return v;
}

// Versao Double (ponteiro)
int * double2arrayint(double *t, int *size, int numbers_needed, int size_vec){
  int alloc_size = (numbers_needed + 2 > size_vec) ? (numbers_needed + 2) : size_vec;
  if (alloc_size < MIN_SAFE_SIZE) alloc_size = MIN_SAFE_SIZE;

  int *v = (int *) R_alloc(alloc_size, sizeof(int));
  std::memset(v, 0, alloc_size * sizeof(int));

  int t_vec = 0;
  for(int j = numbers_needed ; j >= 0; j--){
    double base = std::pow(10, j);
    int n = (int)(*t / base);
    *t -= (n * base);
    v[t_vec] = n;
    t_vec++;
  }
  *(size) = t_vec;
  return v;
}

// Versao Double (valor)
int * double2arrayint(double t, int *size, int numbers_needed, int size_vec){
  int alloc_size = (numbers_needed + 2 > size_vec) ? (numbers_needed + 2) : size_vec;
  if (alloc_size < MIN_SAFE_SIZE) alloc_size = MIN_SAFE_SIZE;

  int *v = (int *) R_alloc(alloc_size, sizeof(int));
  std::memset(v, 0, alloc_size * sizeof(int));

  int t_vec = 0;
  for(int j = numbers_needed ; j >= 0; j--){
    double base = std::pow(10, j);
    int n = (int)(t / base);
    t -= (n * base);
    v[t_vec] = n;
    t_vec++;
  }
  *(size) = t_vec;
  return v;
}
