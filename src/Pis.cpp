#include "Pis.h"


unsigned int Pis::sizeRaValidate(){
  return size_ra - 1;
}
unsigned int Pis::sizeRaGenerateLastDigit(){
  return size_ra - 2;
}

int Pis::generate_last_digit(){
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

int Pis::validate(){
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
    error = 0;
  }else{
    r = 0;
    error = 1;
  }
  return r;
}





