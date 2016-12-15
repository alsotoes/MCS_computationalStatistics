rm(list=ls())
set.seed(28112016)
n = 100
A <- 1
B <- 5
C <- 3
x <- seq(-10, 10, length.out = n)
y <- A*x + B + rnorm(n, mean=0, sd=C)

data  <- read.csv("cheese.csv")

plot(x, y, main="Datos")

loglikelihood <- function(x, y, theta){
  a   <- theta[1]
  b   <- theta[2]
  std <- theta[3]
  
  yy <- a*x+b
  singleLikelihood <- dnorm(y, mean=yy, sd=std, log=T)
  sumll <- sum(singleLikelihood)
  return(sumll)
}

logprior <- function(theta){
  a   <- theta[1]
  b   <- theta[2]
  std <- theta[3]
  
  aa <- dunif(a, min=0, max=50, log=T)
  bb <- dnorm(b, sd=5, log=T)     #jump size
  std0 <- dunif(std, min=0, max=50, log=T)
  return(aa+bb+std0)
}

logposteriori <- function(x, y, theta){
  return(loglikelihood(x, y, theta) + logprior(theta))
}

proposal <- function(theta){
  a   <- theta[1]
  b   <- theta[2]
  std <- theta[3]
  #sd is jump size
  return(rnorm(3, mean=c(a,b,std), sd=c(0.1, 0.5, 0.3)))
}

runMCMC <- function(x, y, startValue, iterations){
  chain <- array(dim = c(iterations+1,3))
  chain[1,] <- startValue
  for (i in 1:iterations){
    prop <- proposal(chain[i,])
    probab <- exp(logposteriori(x,y,prop)- logposteriori(x,y,chain[i,]))
    if(runif(1) < probab){
      chain[i+1, ] = prop
    } else {
      chain[i+1, ] = chain[i, ]
      #i <- i-1
    }
    print(chain[i,])
    print(i)
    #readkey()
  }
  return(chain)
}

readkey <- function()
{
  cat ("Press [enter] to continue")
  line <- readline()
}

theta0 <- c(1,1,1)
#chain <- runMCMC(x, y, startValue = theta0, iterations=10000)
chain <- runMCMC(data$Acetic, data$taste, startValue = theta0, iterations=10000)
burnIn <- 5000
acceptance <- 1 - mean(duplicated(chain[-(1:burnIn), ]))

###Summary
par(mfrow=c(2,3))
hist(chain[-(1:burnIn), 1], nclass=30, main='Posterior of a', xlab='True value = red line')
abline(v=mean(chain[-(1:burnIn), 1]))
abline(v=A, col='red')
hist(chain[-(1:burnIn), 2], nclass=30, main='Posterior of b', xlab='True value = red line')
abline(v=mean(chain[-(1:burnIn), 2]))
abline(v=B, col='red')
hist(chain[-(1:burnIn), 3], nclass=30, main='Posterior of sd', xlab='True value = red line')
abline(v=mean(chain[-(1:burnIn), 3]))
abline(v=C, col='red')

plot(chain[-(1:burnIn),1], type = "l", xlab="True value = red line" , main = "Chain values of a" )
abline(h = A, col="red" )
plot(chain[-(1:burnIn),2], type = "l", xlab="True value = red line" , main = "Chain values of b" )
abline(h = B, col="red" )
plot(chain[-(1:burnIn),3], type = "l", xlab="True value = red line" , main = "Chain values of sd" )
abline(h = C, col="red" )

summary(lm(y~x))
