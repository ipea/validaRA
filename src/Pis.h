#ifndef PIS_H
#define PIS_H
#include "ra.h"

#define tpis_cpf 11
using namespace Rcpp;
static const int digito_pis[] = {3, 2, 9, 8, 7, 6, 5, 4, 3, 2, 0};

class Pis: public  Ra{
public:
  Pis(int *digits_value, unsigned int size): Ra(digits_value, size){ size_ra = 11; };
  Pis() : Ra() { size_ra = 11; };
  unsigned int sizeRaValidate();
  unsigned int sizeRaGenerateLastDigit();
  int generate_last_digit();
  int validate();
};

#endif



