rm(list=ls())
options(digits=4)
set.seed(1011)
res <- data.frame() #used to store results

trapezoid <- function(N, a, b, FUN){
  #FUN is the function for integrate: R^di --> R
  #a,b are dim-dimensional vectors specifying the limits of integration
  dim <- length(a)
  x <- seq(a[1], b[1], (b[1]-a[1])/N)
  if(dim == 1){
    fi <- sapply(x, FUN)
  } else{
    fi <- sapply(x, function(x){
      trapezoid(N, a[-1], b[-1], function(y) FUN(c(x,y)))
    })
  }
  return(((b[1]-a[1])/(2*N))*sum(fi[-1]+fi[-(N+1)]))
}

mc.intervals <- function(phi, N, x.dens=runif, alpha=0.05){
  
  results.list <- lapply(N, function(nsim){
    x <- sapply(FUN=x.dens, nsim)
    phiX <- sapply(x, phi)
    estim <- mean(phiX)
    S2 <- var(phiX)
    quant <- qnorm(alpha/2, lower.tail = F)
    int.upper <- estim + sqrt(S2/nsim)*quant
    int.lower <- estim - sqrt(S2/nsim)*quant
    return(data.frame(N=nsim, Estimate=estim, LI=int.lower, UI=int.upper))
  })
}

#different dimensions sizes
for(m in c(1,5)){
  real.value <- (pnorm(2)-pnorm(-2))^m
  #different partition sizes (trapezoid)/nsim (monteCarlo)
  for(N in 2:7){ #recall trapezoid needs nsim=N^m points
    FUN <- function(x) (2*pi)^(-m/2)*exp((-1/2)*t(x)%*%x)
    a <- rep(-2,m)
    b <- rep(2, m)
    estim.num <- trapezoid(N, a, b, FUN)
    #X.dens must be a list of m-dimensional vector of Unif(-2,2)
    X.dens <- function(nsim) replicate(nsim, runif(m, -2,2), simplify=F)
    Phi <- function(x) (4^m)*FUN(x)
    #For M^c we evaluate in N^m points instead, so that it is comparable
    estim.MC <- data.frame(mc.intervals(Phi, N^m, X.dens))
    res.temp <- data.frame(Dim=m, Nsim = N^m, Real = real.value, Num=estim.num,
                           Err.Num = abs(estim.num - real.value),
                           MC = estim.MC$Estimate,
                           MC.LI = estim.MC$LI, MC.UI=estim.MC$UI,
                           Err.MC=abs(estim.MC$Estimate-real.value)
                          )
    res <- rbind(res, res.temp)
  }
}
res

require(ggplot2)
ggplot(subset(res, Dim==1), aes(x=Nsim)) +
  geom_line(aes(y=Err.Num, colour="Trapezoidal")) +
  geom_line(aes(y=Err.MC, colour="MonteCarlo")) +
  scale_colour_manual("Error", values=c("red", "blue")) +
  ggtitle("Dimensión = 1")

ggplot(subset(res, Dim==5), aes(x=log(Nsim,5))) +
  geom_line(aes(y=Err.Num, colour="Trapezoidal")) +
  geom_line(aes(y=Err.MC, colour ="MonteCarlo")) +
  scale_colour_manual("Error", values=c("red", "blue")) +
  ggtitle("Dimensión = 5")


