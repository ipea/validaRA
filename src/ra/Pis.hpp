#ifndef PIS_H
#define PIS_H
#include "Ra.hpp"


static const int digito_pis[] = {3, 2, 9, 8, 7, 6, 5, 4, 3, 2, 0};

class Pis: public  Ra{
public:
  Pis(int *digits_value, unsigned int size): Ra(digits_value, size){ this->size_ra = 11; };
  Pis() : Ra() { this->size_ra = 11; };

  unsigned int sizeRaValidate(){
    return size_ra - 1;
  }
  unsigned int sizeRaGenerateLastDigit(){
    return size_ra - 2;
  }

  int generate_last_digit(){
    int result = 0;
    for(unsigned int i = 0; i < size_ra; i++){
      result += digits[i] * digito_pis[i];
    }
    result = (result * 10) % size_ra;
    if(result == 10) result = 0;
    digits[10] = result;
    size++;
    return result;
  }

  int validate(){
    int result = 0;
    for(unsigned int i = 0; i < size; i++){
      result += digits[i] * digito_pis[i];
    }
    result = (result * 10) % size_ra;
    if(result == 10) result = 0;
    return validate_digit(result, 10, 1);
  }
};

#endif



