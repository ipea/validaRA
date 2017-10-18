#ifndef CNPJ_H
#define CNPJ_H
#include "Ra.hpp"



static const int primeiro_digito_cnpj[] = {5, 4, 3, 2, 9, 8, 7, 6, 5, 4, 3, 2, 0, 0};
static const int segundo_digito_cnpj[] =  {6, 5, 4, 3, 2, 9, 8, 7, 6, 5, 4, 3, 2, 0};

class Cnpj: public  Ra{
public:
  Cnpj(int *digits_value, unsigned int size): Ra(digits_value, size){ this->size_ra = 14; };
  Cnpj() : Ra() { this->size_ra = 14; };
  unsigned int sizeRaValidate(){
    return size_ra - 1;
  }
  unsigned int sizeRaGenerateLastDigit(){
    return size_ra - 2;
  }

  int generate_last_digit(){
    int result = 0;
    for(unsigned int i = 0; i < size_ra; i++){
      result += digits[i] * primeiro_digito_cnpj[i];
    }
    result = (result * 10) % size_ra;
    if(result == 10) result = 0;
    digits[12] = result;
    size++;
    result = 0;
    for(unsigned int i = 0; i < size_ra; i++){
      result += digits[i] * segundo_digito_cnpj[i];
    }
    result = (result * 10) % size_ra;
    if(result == 10) result = 0;
    digits[13] = result;
    size++;
    return result;
  }

  int validate(){
    int result = 0, result1 = 0;
    for(unsigned int i = 0; i < size; i++){
      result += digits[i] * primeiro_digito_cnpj[i];
      result1 += digits[i] * segundo_digito_cnpj[i];
    }
    result = (result * 10) % 11;
    if(result == 10) result = 0;
    result1 = (result1 * 10) % 11;
    if(result1 == 10) result1 = 0;
    int r = validate_digit(result, 12, 1);
    r = r == 1 ? validate_digit(result1, 13, 2) : r;
    return r;
  }
};




#endif



