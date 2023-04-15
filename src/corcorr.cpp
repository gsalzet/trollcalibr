// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-

// we only include RcppArmadillo.h which pulls Rcpp.h in for us
#include "RcppArmadillo.h"

// [[Rcpp::depends(RcppArmadillo)]]

arma::mat sw (arma::mat obj, int i, int j, int k) {
  double tmp;
  tmp = obj(i, k);
  obj(i, k) = obj(j, k);
  obj(j, k) = tmp;
  return(obj);
}

//' @title internal Correlation Matrix Correction
//'
//' @description
//' This function changes the order in which data is organized in order
//' to force the correlation matrix to a prescribed value. This implementation
//' uses the Hungtington-Lyrintzis algorithm.
//'
//' @name corcorr
//'
//' @param vars mat. WIP LHS matrix.
//' @param cor mat. Correlation matrix.
//' @param l int. Convergence tolerance.
//' @param FLAGSTOP int. Flag to check convergence .
//'
//' @return Void with outputs files written in the defined folder.
//'
//' @export
// [[Rcpp::export]]
Rcpp::List  corcorr(arma::mat vars, arma::mat cor,int l, int FLAGSTOP) {
  
  int i = 0, j = 0, k = 0, m = 0;
  double mean, sd, sum =0, sq_sum =0;
  int mini = 0, minj = 0;
  double minE = 0, E, Tj;
  
  int N = vars.n_rows;
  int M = vars.n_cols;
  
  arma::mat tmp(N,M);
  
  /* Normaliza as variaveis */
  for (j=0;j< l;j++) {
    sum=0; sq_sum=0;
    for(i = 0; i < N; ++i) {
      sum += vars(i,j);
      sq_sum += vars(i,j) * vars(i,j);
    }
    mean = sum / (N);
    sd = sqrt(sq_sum / N - mean * mean);
    for (i = 0; i< N; i++)
      tmp(i,j) = (vars(i,j)-mean)/sd;
  }
  
  /* Calcula o valor de E antes de realizar qualquer troca */
  for (m=0; m < l-1;m++) {
    Tj = 0;
    for (k=0;k<N;k++)
      Tj += tmp(k,l-1) * tmp(k,m);
    minE += (Tj/(N) - cor(l-1,m))*(Tj/(N) - cor(l-1,m));
  }
  
  for (i =0; i < N-1; i++) {
    for (j = i+1; j < N; j++) {
      E =0;
      /* Troca o valor de R[i] e R[j] */
      tmp = sw(tmp, i, j, (l-1));
      for (m=0; m < l-1;m++) {
        Tj = 0;
        for (k=0;k<N;k++) {
          Tj += tmp(k ,l-1) * tmp(k,m);
        }
        E += (Tj/(N) - cor(l-1,m))*(Tj/(N) - cor(l-1,m));
      }
      /* trabalha com E aqui */
      if (E < minE) {
        mini = i;
        minj = j;
        minE = E;
      }
      /* Finalmente, destroca i e j */
      tmp = sw(tmp, i, j, (l-1));
      
    }
  }
    
  if (mini == 0 && minj == 0) {
    FLAGSTOP = 1;
  } else {
    /* Troca o valor da variavel na posicao i e j e finaliza */
    vars = sw(vars, mini, minj, (l-1));
  }
  
    
    return Rcpp::List::create(Rcpp::Named("vars")=vars,
                              Rcpp::Named("FLAGSTOP")=FLAGSTOP);
}
