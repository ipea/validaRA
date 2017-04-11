#include "Cpf.h"
#include "Pis.h"

Ra * factoryRa(const char *t){
  Ra *ra;
  if(strcmp(t,"pis") == 0){
    //std::cout << "Entrou aqui" << std::endl;
    ra = new Pis();
  }else if(strcmp(t,"cpf") == 0){
    ra = new Cpf();
  }else{
    ra = new Pis();
  }
  return ra;
}



