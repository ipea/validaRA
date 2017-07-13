#ifndef CNS_H
#define CNS_H
#include "Ra.hpp"

#define tcns 15
using namespace Rcpp;
static const int digito_cns[] = {15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1};

class CNS: public  Ra{
public:
  CNS(int *digits_value, unsigned int size): Ra(digits_value, size){ size_ra = tcns; };
  CNS() : Ra() { size_ra = tcns; };

  unsigned int sizeRaValidate(){
    return size_ra - 1;
  }
  unsigned int sizeRaGenerateLastDigit(){
    return size_ra - 2;
  }

  int generate_last_digit(){
    int result = 0;
    for(int i = 0; i < tcns; i++){
      result += digits[i] * digito_cns[i];
    }
    result = (result * 10) % tcns;
    if(result == 10) result = 0;
    digits[10] = result;
    size++;
    return result;
  }

  int sum_digits(int tloop){
    int result = 0;
    for(unsigned int i = 0; i < tloop; i++){
      result += digits[i] * digito_cns[i];
    }
    return result;
  }

  int validate(){
    int r = 0;
    int result = 0;
    if(size < tcns){
      std::fill_n(digits + size, tcns - size, -1);
    }

    //std::cout << "Digits" <<  digits[10] << std::endl;
    if(digits[0] == 7 || digits[0] == 8|| digits[0] == 9){
      result = sum_digits(15);
      result = result  % 11;
      if(result != 0){
        r = 0;
        error = 1;
      }else{
        r = 1;
        error = 0;
      }
      return r;
    }else{
      result = sum_digits(11);
      double resto = result % 11;
      double dv = 11 - resto;
      if(dv == 11){
        dv = 0;
      }
      if(dv == 10){
        result += 2;
        resto = result % 11;
        dv = 11 - resto;
        if(digits[13] == 1 && digits[12] == 0 && digits[11] == 0 && digits[14] == dv){
          r = 1;
          error = 0;
        }else{
          r = 0;
          error = 1;
        }
      }else{
        if(digits[13] == 0 && digits[12] == 0 && digits[11] == 0 && digits[14] == dv){
          r = 1;
          error = 0;
        }else{
          r = 0;
          error = 2;
        }
      }

    }
    return r;
  }
};

#endif



