fun <- function(x){
aux <- num*sqrt(10*x-x^2-24); 
aux[is.nan(aux)] <- 0; 
return(aux)
}

fun2 <- function(x){
  aux <- sqrt(4-x^2)
  aux[is.nan(aux)] <- 0
  return (aux)
}

from <- -2
to <- 2
n <- 1000
x <- runif(n, from, to)
to1 <- to-from 


(monteCarlo <- mean(fun(x)))

(monteCarlo <- to1*mean(fun2(x)))
