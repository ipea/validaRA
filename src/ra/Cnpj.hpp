#ifndef CNPJ_H
#define CNPJ_H
#include "Ra.hpp"

#define tcnpj 14
using namespace Rcpp;
static const int primeiro_digito_cnpj[] = {5, 4, 3, 2, 9, 8, 7, 6, 5, 4, 3, 2, 0, 0};
static const int segundo_digito_cnpj[] =  {6, 5, 4, 3, 2, 9, 8, 7, 6, 5, 4, 3, 2, 0};

class Cnpj: public  Ra{
public:
  Cnpj(int *digits_value, unsigned int size): Ra(digits_value, size){ size_ra = 14; };
  Cnpj() : Ra() { size_ra = 14; };
  unsigned int sizeRaValidate(){
    return size_ra - 1;
  }
  unsigned int sizeRaGenerateLastDigit(){
    return size_ra - 2;
  }

  int generate_last_digit(){
    int result = 0;
    for(int i = 0; i < tcnpj; i++){
      result += digits[i] * primeiro_digito_cnpj[i];
    }
    result = (result * 10) % tcnpj;
    if(result == 10) result = 0;
    digits[12] = result;
    size++;
    result = 0;
    for(int i = 0; i < tcnpj; i++){
      result += digits[i] * segundo_digito_cnpj[i];
    }
    result = (result * 10) % tcnpj;
    if(result == 10) result = 0;
    digits[13] = result;
    size++;
    return result;
  }

  int validate(){
    int r = 0;
    if(all_equal()){
      error = 1;
      return r;
    }
    int result = 0;
    int result1 = 0;
    if(size < tcnpj){
      std::fill_n(digits + size, tcnpj - size, -1);
    }
    //std::cout << "Digits" <<  digits[10] << std::endl;
    for(unsigned int i = 0; i < size; i++){
      result += digits[i] * primeiro_digito_cnpj[i];
      result1 += digits[i] * segundo_digito_cnpj[i];
    }
    result = (result * 10) % 11;
    if(result == 10) result = 0;
    //std::cout << "R " << result << " d " << digits[12] << std::endl;
    if(result == digits[12]){
      r = 1;
      error = 0;
    }else{
      r = 0;
      error = 1;
      return r;
    }

    result1 = (result1 * 10) % 11;
    if(result1 == 10) result1 = 0;
    if(result1 == digits[13]){
      r = 1;
      error = 0;
    }else{
      r = 0;
      error = 2;
    }
    return r;
  }
};




#endif



