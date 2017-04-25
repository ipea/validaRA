#ifndef CPF_H
#define CPF_H
#include "ra.h"

#define tpis_cpf 11
using namespace Rcpp;
static const int arr_primeiro_digito[] = {10,  9, 8, 7, 6, 5, 4, 3, 2, 0, 0};
static const int arr_segundo_digito[] =  {11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 0};

class Cpf: public  Ra{
public:
  Cpf(int *digits_value, unsigned int size): Ra(digits_value, size){ size_ra = 11; };
  Cpf() : Ra() { size_ra = 11; };
  unsigned int sizeRaValidate();
  unsigned int sizeRaGenerateLastDigit();
  int generate_last_digit();
  bool validate();
};




#endif



