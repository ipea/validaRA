#ifndef CEI_H
#define CEI_H
#include "Ra.hpp"

// Pesos oficiais para o cálculo do dígito verificador do CEI
static const int arr_cei_pesos[] = {7, 4, 1, 8, 5, 2, 1, 6, 3, 7, 4};

class Cei: public Ra {
public:
  Cei(int *digits_value, unsigned int size): Ra(digits_value, size){ size_ra = 12; };
  Cei() : Ra() { size_ra = 12; };
  
  unsigned int sizeRaValidate(){
    return size_ra - 1; // 11 dígitos base para validar
  }
  unsigned int sizeRaGenerateLastDigit(){
    return size_ra - 1; // 11 dígitos base para gerar o 12º
  }
  
  // Lógica principal do algoritmo do CEI
  int calc_dv() {
    int soma = 0;
    
    // 1. Multiplica os 11 primeiros dígitos pelos pesos e soma
    for(unsigned int i = 0; i < 11; i++){
      soma += digits[i] * arr_cei_pesos[i];
    }
    
    // 2. Isola a dezena e a unidade da soma total
    int dezena = (soma % 100) / 10;
    int unidade = soma % 10;
    
    // 3. Soma a dezena com a unidade
    int soma_aux = dezena + unidade;
    
    // 4. Calcula o dígito verificador (10 - resto do módulo 10)
    int resto = soma_aux % 10;
    int dv = (10 - resto) % 10; // O '% 10' no final garante que se der 10, vira 0.
    
    return dv;
  }
  
  int generate_last_digit(){
    int dv = calc_dv();
    digits[11] = dv;
    size++;
    return dv;
  }
  
  int validate(){
    int dv = calc_dv();
    // Verifica se o DV calculado bate com o 12º dígito (índice 11)
    return validate_digit(dv, 11, 1);
  }
};

#endif