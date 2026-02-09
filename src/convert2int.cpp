#include "convert2int.h"
#include <R.h>
#include <Rinternals.h>
#include <cstring>
#include <cmath>
#include <cctype>

// =========================================================
// CORRECAO CRITICA:
// 1. Uso de R_alloc em vez de malloc para evitar vazamento de memoria.
// 2. Calculo dinamico do tamanho de alocacao para evitar buffer overflow (escrita fora da memoria).
// =========================================================

// =========================================================
// CORRECAO DE CRASH (Heap Corruption):
// Definimos um tamanho minimo seguro (MIN_SAFE_SIZE = 16).
// Isso garante que mesmo se o usuario passar string vazia "" ou curta "123",
// o validador (que espera 11 ou 14 digitos) nao le memory fora do array.
// =========================================================
// fix para https://github.com/ipea/validaRA/issues/16

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
    // Substituido boost::lexical_cast por isdigit padrao (mais leve e seguro)
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
  // CORRECAO: Se numbers_needed for maior que size_vec (ex: 11), expande a alocacao.
  // Isso previne o crash "out of memory write".
  int alloc_size = (numbers_needed + 2 > size_vec) ? (numbers_needed + 2) : size_vec;
  int *v = (int *) R_alloc(alloc_size, sizeof(int));

  int t_vec = 0;
  for(int j = numbers_needed ; j >= 0; j--){
    double base = std::pow(10, j);
    long long val = *t;
    int n = (int)(val / (long long)base);
    // Preserva o comportamento original de modificar o valor apontado
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
  int *v = (int *) R_alloc(alloc_size, sizeof(int));

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
  int *v = (int *) R_alloc(alloc_size, sizeof(int));

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
  int *v = (int *) R_alloc(alloc_size, sizeof(int));

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
