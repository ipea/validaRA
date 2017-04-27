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
    Ra(){
      this->size = 0;
      digits = NULL;
      error = 0;
    };
    Ra(int *digits_value, unsigned int size){
      this->size = 0;
      error = 0;
      set_digits(digits_value, size);
    }
    virtual int generate_last_digit() = 0;
    virtual int validate() = 0;
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
    bool all_equal();
    friend ostream& operator<<(ostream& os, const Ra& ra);
};

#endif



