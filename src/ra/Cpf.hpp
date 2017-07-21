#ifndef CPF_H
#define CPF_H
#include "Ra.hpp"

#define tpis_cpf 11
using namespace Rcpp;
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
    for(int i = 0; i < tpis_cpf; i++){
      result += digits[i] * arr_primeiro_digito[i];
    }
    result = (result * 10) % tpis_cpf;
    if(result == 10) result = 0;
    digits[9] = result;
    size++;
    result = 0;
    for(int i = 0; i < tpis_cpf; i++){
      result += digits[i] * arr_segundo_digito[i];
    }
    result = (result * 10) % tpis_cpf;
    if(result == 10) result = 0;
    digits[10] = result;
    size++;
    return result;
  }

  int validate(){
    int r = 0;
    if(all_equal()){
      error = 3;
      return r;
    }
    int result = 0;
    int result1 = 0;
    if(size < tpis_cpf){
      std::fill_n(digits + size, tpis_cpf - size, -1);
    }
    //std::cout << "Digits" <<  digits[10] << std::endl;
    for(unsigned int i = 0; i < size; i++){
      result += digits[i] * arr_primeiro_digito[i];
      result1 += digits[i] * arr_segundo_digito[i];
    }
    result = (result * 10) % tpis_cpf;
    if(result == 10) result = 0;
    if(result == digits[9]){
      r = 1;
      error = 0;
    }else{
      r = 0;
      error = 1;
      return r;
    }

    result1 = (result1 * 10) % tpis_cpf;
    if(result1 == 10) result1 = 0;
    if(result1 == digits[10]){
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



