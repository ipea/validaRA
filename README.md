
<!-- README.md is generated from README.Rmd. Please edit that file -->
Valida RA
=========

work in progress
----------------

### NEW:

-   Generate last digit of pis

### COMMING SOON:

-   Generate last digits for any RA document

Description
-----------

This package contains functions to work with brazilian documents ids as CPF, PIS and CNPJ. It can validate them e outputing the result on different ways. Also, it can handle situations where the ids has just its prefix(first ten digits, on cpf, for example) and it is needed to generate the last one.

Basic Usage
-----------

``` r
##############
# Censo Demográfico 2000
result <- valiada_doc(c("788.346.838-38"), type = "cpf")
```
