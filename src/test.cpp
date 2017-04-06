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
  Pis p;
  for(int i = 0; i < 11; i++){
    p.push(digito[i]);
  }
  std::cout << p.int2char() << std::endl;
}

// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::export]]
void test_classes(){
  test_constructor_pis();
  //test_intTochar();
  // int digito[] = {1,2,0,7,4,3,6,2,7,8,3};
  // int digito2[] = {1,2,0,7,4,3,6,2,7,8,0};
  // Pis p;
  // for(int i = 0; i < 11; i++){
  //   p.push(digito[i]);
  // }
  //
  // std::cout << p.int2char() << std::endl;
  // //std::cout << p.validate() << std::endl;
  // //digito[3] = 8;
  //
  // Pis p2(digito, 11);
  // std::cout << p2.validate() << std::endl;
  // Pis p3(digito2,11);
  // std::cout << p3 << std::endl;
  // std::cout << p3.generate_last_digit() << std::endl;



}



