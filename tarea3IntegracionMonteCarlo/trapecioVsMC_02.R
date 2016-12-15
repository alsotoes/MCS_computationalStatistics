fun <- function (x) {return(4/(1+x^2))}

trapezoid <- function(n, a, b, FUN){
  dim <- length(a)
  x <- seq(a[1], b[1], (b[1]-a[1])/n)
  if(dim == 1){
    fi <- sapply(x, FUN)
  } else{
    fi <- sapply(x, function(x){
      trapezoid(n, a[-1], b[-1], function(y) FUN(c(x,y)))
    })
  }
  return(((b[1]-a[1])/(2*n))*sum(fi[-1]+fi[-(n+1)]))
}

MonteCarlo <- function(n, from, to, fun){
  aux <- sapply(runif(n, from, to), fun)
  return((to-from)*mean(aux))
}

mcIntervals <- function(nn, from, to, fun, alfa=0.05){
  lower <- {}
  upper <- {}
  mC <- {}
  
  for (n in 1:nn){
    aux <- (to-from)*mean(sapply(runif(n, from, to), fun))
    mC <- c(mC, aux)
    S <- sd(mC)
    error <- qnorm(1-alfa)*S/sqrt(length(mC))
    lo <- aux-error
    up <- aux+error
    lower <- c(lower, lo)
    upper <- c(upper, up)
  }
  aux <- data.frame(lower=lower, mC=mC, upper=upper)
  return(aux)
}

# mc.intervals <- function(phi, N, x.dens=runif, alpha=0.05){
#   
#   results.list <- lapply(N, function(nsim){
#     x <- sapply(FUN=x.dens, nsim)
#     phiX <- sapply(x, phi)
#     estim <- mean(phiX)
#     S2 <- var(phiX)
#     quant <- qnorm(alpha/2, lower.tail = F)
#     int.upper <- estim + sqrt(S2/nsim)*quant
#     int.lower <- estim - sqrt(S2/nsim)*quant
#     return(data.frame(N=nsim, Estimate=estim, LI=int.lower, UI=int.upper))
#   })
# }

from=c(0)
to = c(2)
nn = 1000
print(paste("Trap: ", trapezoid(nn, from, to, fun)))
print(paste("MC: ", MonteCarlo(nn, from, to, fun)))
print(paste("Intervalos:", mcIntervals(nn=10, from, to, fun)))
