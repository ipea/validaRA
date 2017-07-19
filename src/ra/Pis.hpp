#ifndef PIS_H
#define PIS_H
#include "Ra.hpp"

#define tpis_cpf 11
using namespace Rcpp;
static const int digito_pis[] = {3, 2, 9, 8, 7, 6, 5, 4, 3, 2, 0};

class Pis: public  Ra{
public:
  Pis(int *digits_value, unsigned int size): Ra(digits_value, size){ size_ra = 11; };
  Pis() : Ra() { size_ra = 11; };
  unsigned int sizeRaValidate(){
    return size_ra - 1;
  }
  unsigned int sizeRaGenerateLastDigit(){
    return size_ra - 2;
  }

  int generate_last_digit(){
    int result = 0;
    for(int i = 0; i < tpis_cpf; i++){
      result += digits[i] * digito_pis[i];
    }
    result = (result * 10) % tpis_cpf;
    if(result == 10) result = 0;
    digits[10] = result;
    size++;
    return result;
  }

  int validate(){
    int r = 0;
    int result = 0;
    if(size < tpis_cpf){
      std::fill_n(digits + size, tpis_cpf - size, -1);
    }
    //std::cout << "Digits" <<  digits[10] << std::endl;
    for(unsigned int i = 0; i < size; i++){
      result += digits[i] * digito_pis[i];
    }
    result = (result * 10) % tpis_cpf;
    if(result == 10) result = 0;
    if(result == digits[10]){
      r = 1;
      this->error = 0;
    }else{
      r = 0;
      this->error = 1;
    }
    return r;
  }
};

#endif



