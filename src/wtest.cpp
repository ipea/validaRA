#include "Pis.h"


void test_constructor_pis(){
  int digito[] = {1,2,0,7,4,3,6,2,7,8,3};
  Pis p;
  for(int i = 0; i < 11; i++){
    p.push(digito[i]);
  }
  std::cout << p << std::endl;
}

void test_intTochar(){
  int digito[] = {1,2,0,7,4,3,6,2,7,8,3};
  Pis p(digito, 11);
  std::cout << p.int2char() << std::endl;
}

void test_intToBit64(){
  int digito[] = {1,2,0,7,4,3,6,2,7,8,3};
  Pis p;
  for(int i = 0; i < 11; i++){
    p.push(digito[i]);
  }
  long long n = p.int2bit64();
  std::cout << n << std::endl;
}



// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::export]]
void test_classes(){
  std::cout << "Test int to char " << std::endl;
  test_intTochar();
  std::cout << "Test int to Bit64 " << std::endl;
  test_intToBit64();
  std::cout << "Test constructor " << std::endl;
  test_constructor_pis();

}



