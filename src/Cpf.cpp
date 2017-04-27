#include "Cpf.h"

unsigned int Cpf::sizeRaValidate(){
  return size_ra - 1;
}
unsigned int Cpf::sizeRaGenerateLastDigit(){
  return size_ra - 2;
}

int Cpf::generate_last_digit(){
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

int Cpf::validate(){
  int r = 0;
  if(all_equal()){
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




