#ifndef CNPJ_H
#define CNPJ_H
#include "ra.h"

#define tcnpj 14
using namespace Rcpp;
static const int primeiro_digito_cnpj[] = {5, 4, 3, 2, 9, 8, 7, 6, 5, 4, 3, 2, 0, 0};
static const int segundo_digito_cnpj[] =  {6, 5, 4, 3, 2, 9, 8, 7, 6, 5, 4, 3, 2, 0};

class Cnpj: public  Ra{
public:
  Cnpj(int *digits_value, unsigned int size): Ra(digits_value, size){ size_ra = 14; };
  Cnpj() : Ra() { size_ra = 14; };
  unsigned int sizeRaValidate();
  unsigned int sizeRaGenerateLastDigit();
  int generate_last_digit();
  int validate();
};




#endif



