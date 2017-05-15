#include "boost.h"

Ra * factoryRa(const char *t){
  Ra *ra;
  if(strcmp(t,"pis") == 0){
    //std::cout << "Entrou aqui" << std::endl;
    ra = new Pis();
  }else if(strcmp(t,"cpf") == 0){
    ra = new Cpf();
  }else if(strcmp(t,"cnpj") == 0){
    ra = new Cnpj();
  }else{
    ra = new Cpf();
  }
  return ra;
}



