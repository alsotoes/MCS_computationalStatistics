rm(list=ls())
set.seed(28112016)
n = 100

data  <- read.csv("cheese.csv")
nms <- names(data)
#data <- as.data.frame(scale(data))
#colnames(data) <- nms

#plot(data$Acetic, data$taste, main="Acetic")
#plot(data$H2S, data$taste, main="H2S")
#plot(data$Lactic, data$taste, main="Lactic")

loglikelihood <- function(x1,x2,x3,y,theta){
  a   <- theta[1]
  b   <- theta[2]
  c   <- theta[3]
  d   <- theta[4]
  std <- theta[5]
  
  yy <- a*x1 + b*x2 + c*x3 + d 
  singleLikelihood <- dnorm(y, mean=yy, sd=std, log=T)
  sumll <- sum(singleLikelihood)
  return(sumll)
}

logprior <- function(theta){
  a   <- theta[1]
  b   <- theta[2]
  c   <- theta[3]
  d   <- theta[4]
  std <- theta[5]
  
  aa <- dunif(a, min=0, max=50, log=T)
  bb <- dunif(b, min=0, max=50, log=T)
  cc <- dunif(c, min=0, max=50, log=T)
  dd <- dnorm(b, sd=5, log=T)     #jump size
  std0 <- dunif(std, min=0, max=50, log=T)
  return(aa+bb+cc+dd+std0)
}

logposteriori <- function(x1, x2, x3, y, theta){
  return(loglikelihood(x1, x2, x3, y, theta) + logprior(theta))
}

proposal <- function(theta){
  a   <- theta[1]
  b   <- theta[2]
  c   <- theta[3]
  d   <- theta[4]
  std <- theta[5]
  #sd is jump size
  return(rnorm(length(theta), mean=c(a,b,std), sd=c(0.3, 0.2, 0.5)))
}

runMCMC <- function(x1,x2,x3, y, startValue, iterations){
  chain <- array(dim = c(iterations+1,5))
  chain[1,] <- startValue
  for (i in 1:iterations){
    prop <- proposal(chain[i,])
    probab <- exp(logposteriori(x1,x2,x3,y,prop)- logposteriori(x1,x2,x3,y,chain[i,]))
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

theta0 <- c(1,1,1,1,1)
chain <- runMCMC(data$Acetic, data$H2S, data$Lactic, data$taste, startValue = theta0, iterations=50000)
burnIn <- 5000
acceptance <- 1 - mean(duplicated(chain[-(1:burnIn), ]))

###Summary
par(mfrow=c(2,5))
hist(chain[-(1:burnIn), 1], nclass=30, main='Posterior of Acetic', xlab='Values')
abline(v=mean(chain[-(1:burnIn), 1]))

hist(chain[-(1:burnIn), 2], nclass=30, main='Posterior of H2S', xlab='Values')
abline(v=mean(chain[-(1:burnIn), 2]))

hist(chain[-(1:burnIn), 3], nclass=30, main='Posterior of Lactic', xlab='Values')
abline(v=mean(chain[-(1:burnIn), 2]))

hist(chain[-(1:burnIn), 4], nclass=30, main='Posterior of Intercept', xlab='Values')
abline(v=mean(chain[-(1:burnIn), 2]))

hist(chain[-(1:burnIn), 5], nclass=30, main='Posterior of sd', xlab='Values')
abline(v=mean(chain[-(1:burnIn), 3]))


plot(chain[-(1:burnIn),1], type = "l", xlab="Values" , main = "Chain values of Acetic" )

plot(chain[-(1:burnIn),2], type = "l", xlab="Values" , main = "Chain values of H2S" )

plot(chain[-(1:burnIn),3], type = "l", xlab="Values" , main = "Chain values of Lactic" )

plot(chain[-(1:burnIn),4], type = "l", xlab="Values" , main = "Chain values of Itercept" )

plot(chain[-(1:burnIn),5], type = "l", xlab="Values" , main = "Chain values of sd" )


summary(lm(data$taste~data$Acetic + data$H2S + data$Lactic))
