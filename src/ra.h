#ifndef RA_H
#define RA_H
#include "boost.h"
#include <Rcpp.h>
#include <Rdefines.h>
#include <RInternals.h>
#include <math.h>
#include <iostream>
using namespace std;

class Ra{
  protected:
    int * digits;
    int error;
    unsigned int size;
    unsigned int size_ra;
  public:
    Ra(){ this->size = 0; digits = NULL; error = 0; };
    Ra(int *digits_value, unsigned int size){ set_digits(digits_value, size); };
    virtual int generate_last_digit() = 0;
    virtual bool validate() = 0;
    void set_digits(int *p, unsigned int size);
    void set_digits(int *p);
    virtual unsigned int sizeRaValidate() = 0;
    virtual unsigned int sizeRaGenerateLastDigit() = 0;
    int * get_size(){ int * q = (int *)&size; return q;}
    int * get_digits(){return digits;}
    int get_error(){return error; }
    bool has_error(){ return error; }
    void clear(){ this->size = 0;}
    char *  int2char();
    long long int2bit64();
    double int2double();
    void push(int n);
    friend ostream& operator<<(ostream& os, const Ra& ra);
};



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
  this->size = size;
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


#endif



