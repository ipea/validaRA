#ifndef CONVERT2INT_H
#define CONVERT2INT_H
#include "libraries.h"
#include <Rcpp.h>
#include <Rdefines.h>
#include <Rinternals.h>
#include <math.h>

bool is_bit64(SEXP x);

bool is_factor(SEXP x);

int * charxp2arrayint(SEXP x, int *size);

int * bit642arrayint(long long *t, int *size, int numbers_needed, int size_vec = 11);

int * bit642arrayint(long long t, int *size, int numbers_needed, int size_vec = 11);

int * double2arrayint(double *t, int *size, int numbers_needed, int size_vec = 11);

int * double2arrayint(double t, int *size, int numbers_needed, int size_vec = 11);

#endif

