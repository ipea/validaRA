#include "Ra.h"



ostream& operator<<(ostream& os, const Ra& ra){
  os << "class: Ra " <<  std::endl;
  os << "Size: " << ra.size << std::endl;
  os << "Digits: ";
  for(unsigned int i = 0; i < ra.size; i++){
    os << ra.digits[i] << "," ;
  }
  return os;
}

long long Ra::int2bit64(){
  long long num = 0;
  //std::cout << "Size " << this->size << std::endl;
  unsigned int temp_size = this->size - 1;
  for(unsigned int i = 0; i <= temp_size; i++){
    num += (pow(10,i) * digits[temp_size - i]);
    //std::cout << num << std::endl;
  }
  return num;
}

double Ra::int2double(){
  double num = 0;
  //std::cout << "Size " << this->size << std::endl;
  unsigned int temp_size = this->size - 1;
  for(unsigned int i = 0; i <= temp_size; i++){
    num += (pow(10,i) * digits[temp_size - i]);
    //std::cout << num << std::endl;
  }
  return num;
}

char *  Ra::int2char(){
//std::cout << "Size: " << size << std::endl;
  char * c = new char[size + 1];
  for(unsigned int i = 0; i < size; i++){
    try{
      c[i] = boost::lexical_cast<char>(digits[i]) ;
    }catch(...){
      std::cout << "Erro aqui na funcao" << std::endl;
      continue;
    }
  }
  c[size] = '\0';
  return c;
}



void Ra::set_digits(int *p, unsigned int size){
  digits = p;
  this->size = size;
}

void Ra::set_digits(int *p){
  digits = p;
}

void Ra::push(int n){
  if(digits == NULL){
    digits = (int *)malloc(sizeof(int) * this->size_ra) ;
  }
  if(digits != NULL){
    digits[this->size] = n;
  }
  this->size++;
}

bool Ra::all_equal(){
  bool r = true;
  for(unsigned int i = 1; i < size; i++){
    if(digits[i - 1] != digits[i]){
      r = false;
      break;
    }
  }
  return r;
}


