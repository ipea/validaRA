#include "convert2int.h"
#include <R.h>
#include <Rinternals.h>
#include <cstring>
#include <cmath>
#include <cctype>
#include <cstdlib> // Necessario para std::malloc

// =========================================================
// CORRECAO DEFINITIVA (Crash + CRAN Error):
// 1. Usamos std::malloc() em vez de R_alloc().
//    Motivo: O crash -1073740940 ocorre porque a classe 'Ra' tenta fazer free()
//    em um ponteiro gerenciado pelo R (R_alloc). Voltamos para malloc para compatibilidade.
// 2. Mantemos MIN_SAFE_SIZE = 16.
//    Motivo: Evita o erro do CRAN onde strings curtas causavam escrita fora da memoria.
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

  // Usa malloc (compativel com o free() interno da classe Ra)
  int *v = (int *) std::malloc(alloc_size * sizeof(int));

  // Zera a memoria para garantir que digitos faltantes sejam 0
  if(v) std::memset(v, 0, alloc_size * sizeof(int));

  int t_vec = 0;
  if(v) {
    for(int j = 0; j < len_t; j++){
      if(isdigit(t[j])){
        v[t_vec] = t[j] - '0';
        t_vec++;
      }
    }
  }
  *(size) = t_vec;
  return v;
}

// Versao Bit64 (ponteiro)
int * bit642arrayint(long long *t, int *size, int numbers_needed, int size_vec){
  int alloc_size = (numbers_needed + 2 > size_vec) ? (numbers_needed + 2) : size_vec;
  if (alloc_size < MIN_SAFE_SIZE) alloc_size = MIN_SAFE_SIZE; // Garante tamanho minimo

  int *v = (int *) std::malloc(alloc_size * sizeof(int));
  if(v) std::memset(v, 0, alloc_size * sizeof(int));

  int t_vec = 0;
  if(v) {
    for(int j = numbers_needed ; j >= 0; j--){
      double base = std::pow(10, j);
      long long val = *t;
      int n = (int)(val / (long long)base);
      *t -= (n * (long long)base);
      v[t_vec] = n;
      t_vec++;
    }
  }
  *(size) = t_vec;
  return v;
}

// Versao Bit64 (valor)
int * bit642arrayint(long long t, int *size, int numbers_needed, int size_vec){
  int alloc_size = (numbers_needed + 2 > size_vec) ? (numbers_needed + 2) : size_vec;
  if (alloc_size < MIN_SAFE_SIZE) alloc_size = MIN_SAFE_SIZE;

  int *v = (int *) std::malloc(alloc_size * sizeof(int));
  if(v) std::memset(v, 0, alloc_size * sizeof(int));

  int t_vec = 0;
  if(v) {
    for(int j = numbers_needed ; j >= 0; j--){
      double base = std::pow(10, j);
      int n = (int)(t / (long long)base);
      t -= (n * (long long)base);
      v[t_vec] = n;
      t_vec++;
    }
  }
  *(size) = t_vec;
  return v;
}

// Versao Double (ponteiro)
int * double2arrayint(double *t, int *size, int numbers_needed, int size_vec){
  int alloc_size = (numbers_needed + 2 > size_vec) ? (numbers_needed + 2) : size_vec;
  if (alloc_size < MIN_SAFE_SIZE) alloc_size = MIN_SAFE_SIZE;

  int *v = (int *) std::malloc(alloc_size * sizeof(int));
  if(v) std::memset(v, 0, alloc_size * sizeof(int));

  int t_vec = 0;
  if(v) {
    for(int j = numbers_needed ; j >= 0; j--){
      double base = std::pow(10, j);
      int n = (int)(*t / base);
      *t -= (n * base);
      v[t_vec] = n;
      t_vec++;
    }
  }
  *(size) = t_vec;
  return v;
}

// Versao Double (valor)
int * double2arrayint(double t, int *size, int numbers_needed, int size_vec){
  int alloc_size = (numbers_needed + 2 > size_vec) ? (numbers_needed + 2) : size_vec;
  if (alloc_size < MIN_SAFE_SIZE) alloc_size = MIN_SAFE_SIZE;

  int *v = (int *) std::malloc(alloc_size * sizeof(int));
  if(v) std::memset(v, 0, alloc_size * sizeof(int));

  int t_vec = 0;
  if(v) {
    for(int j = numbers_needed ; j >= 0; j--){
      double base = std::pow(10, j);
      int n = (int)(t / base);
      t -= (n * base);
      v[t_vec] = n;
      t_vec++;
    }
  }
  *(size) = t_vec;
  return v;
}
