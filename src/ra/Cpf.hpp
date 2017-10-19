#ifndef CPF_H
#define CPF_H
#include "Ra.hpp"


static const int arr_primeiro_digito[] = {10,  9, 8, 7, 6, 5, 4, 3, 2, 0, 0};
static const int arr_segundo_digito[] =  {11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 0};

class Cpf: public  Ra{
public:
  Cpf(int *digits_value, unsigned int size): Ra(digits_value, size){ size_ra = 11; };
  Cpf() : Ra() { size_ra = 11; };
  unsigned int sizeRaValidate(){
    return size_ra - 1;
  }
  unsigned int sizeRaGenerateLastDigit(){
    return size_ra - 2;
  }

  int generate_last_digit(){
    int result = 0;
    for(unsigned int i = 0; i < size_ra; i++){
      result += digits[i] * arr_primeiro_digito[i];
    }
    result = (result * 10) % size_ra;
    if(result == 10) result = 0;
    digits[9] = result;
    size++;
    result = 0;
    for(unsigned int i = 0; i < size_ra; i++){
      result += digits[i] * arr_segundo_digito[i];
    }
    result = (result * 10) % size_ra;
    if(result == 10) result = 0;
    digits[10] = result;
    size++;
    return result;
  }


  int validate(){
    int result = 0, result1 = 0;
    for(unsigned int i = 0; i < size; i++){
      result += digits[i] * arr_primeiro_digito[i];
      result1 += digits[i] * arr_segundo_digito[i];
    }
    result = (result * 10) % size_ra;
    if(result == 10) result = 0;
    result1 = (result1 * 10) % size_ra;
    if(result1 == 10) result1 = 0;
    int r = validate_digit(result, 9, 1);
    r = r == 1 ? validate_digit(result1, 10, 2) : r;
    return r;
  }
};




#endif



