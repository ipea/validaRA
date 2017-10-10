
<!-- README.md is generated from README.Rmd. Please edit that file -->
Valida RA [![Coverage Status](https://img.shields.io/codecov/c/github/IPEA/validaRA/master.svg)](https://codecov.io/github/IPEA/validaRA?branch=master)
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

Installation
------------

The package is not yet on CRAN, so to install it is needed to use devtools. Devtools can be installed as a normal R package:

``` r
install.packages("devtools") 
```

After that, it is possible install validaRA from the github repository using devtools:

``` r
devtools::install_github("ipea/validaRA") 
```

The package, validaRA, depends on c++ code that has to be compiled, then Rtools must be installed. It can be found [here](https://cran.r-project.org/bin/windows/Rtools/)

Basic Usage
-----------

``` r
# Validanting cpf
result <- valiada_doc(c("788.346.838-38"), type = "cpf")

# Validanting pis
result <- valiada_doc(c("788.346.838-38"), type = "pis")
```
