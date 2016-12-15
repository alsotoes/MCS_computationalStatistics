rm(list=ls())
set.seed(28112016)
trueA <- 1
trueB <- 5
trueSd <- 3
sampleSize <- 100

x <- seq(-10, 10, length.out=sampleSize)
y <- trueA*x + trueB + rnorm(n=sampleSize, mean=0, sd=trueSd)

plot(x, y, main="Test Data")

#loglikelihood
loglikelihood <- function(x, y, theta){
  #needs data: x and y
  a = theta[1]
  b = theta[2]
  sd = theta[3]
  
  pred = a*x + b
  singlelikelihoods = dnorm(y, mean=pred, sd=sd, log=T)
  sumll = sum(singlelikelihoods)
  return(sumll)
}

#Example: plot the likelihood profile of the slope a
slopevalues <- function(x) {return(loglikelihood(x, y, c(x, trueB, trueSd)))}
slopelikelihoods <- lapply(seq(3,7, by=0.05), slopevalues)
plot(seq(3,7, by=0.05), slopelikelihoods, type="l", xlab="values of slope thetaeter a", ylab="Log likelihood")


#prior distribution
logprior <- function(theta){
  a = theta[1]
  b = theta[2]
  sd= theta [3]
  
  aprior = dunif(a, min=0, max = 10, log=T)
  bprior = dnorm(b, sd=5, log=T)
  sdprior = dunif(sd, min=0, max=30, log=T)
  return(aprior+bprior+sdprior)
}

logposterior <- function(x,y,theta){
  return(loglikelihood(x,y,theta) + logprior(theta))
}

##########Metropolis algorithm
proposalFunction <- function(theta){
  return(rnorm(3, mean=theta, sd=c(0.1, 0.5, 0.3)))
}

run_metropolis_MCMC <- function(x,y,startvalue, iterations){
  chain = array(dim=c(iterations+1, 3))
  chain[1,] <- startvalue
  for(i in 1:iterations){
    proposal <- proposalFunction(chain[i,])
    
    probab <- exp(logposterior(x,y,proposal) - logposterior(x,y,chain[i,]))
    if(runif(1) < probab){
      chain[i+1, ] = proposal
    } else{
      chain[i+1, ] = chain[i,]
    }
  }
  return(chain)
}

startvalue <- c(4,0,10)
chain <- run_metropolis_MCMC(x, y, startvalue=startvalue, 10000)

burnIn <- 5000
acceptance = 1 - mean(duplicated(chain[-(1:burnIn),]))

###Summary
par(mfrow=c(2,3))
hist(chain[-(1:burnIn), 1], nclass=30, main='Posterior of a', xlab='True value = red line')
abline(v=mean(chain[-(1:burnIn), 1]))
abline(v=trueA, col='red')
hist(chain[-(1:burnIn), 2], nclass=30, main='Posterior of b', xlab='True value = red line')
abline(v=mean(chain[-(1:burnIn), 2]))
abline(v=trueB, col='red')
hist(chain[-(1:burnIn), 3], nclass=30, main='Posterior of sd', xlab='True value = red line')
abline(v=mean(chain[-(1:burnIn), 3]))
abline(v=trueSd, col='red')

plot(chain[-(1:burnIn),1], type = "l", xlab="True value = red line" , main = "Chain values of a" )
abline(h = trueA, col="red" )
plot(chain[-(1:burnIn),2], type = "l", xlab="True value = red line" , main = "Chain values of b" )
abline(h = trueB, col="red" )
plot(chain[-(1:burnIn),3], type = "l", xlab="True value = red line" , main = "Chain values of sd" )
abline(h = trueSd, col="red" )

summary(lm(y~x))

