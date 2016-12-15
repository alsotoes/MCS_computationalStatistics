rm(list=ls())
if(!require(shiny)) install.packages("shiny")
set.seed(24082016)
library(shiny)

mcIntervals <- function(nn, from, to, fun, alfa=0.05){
  lower <- {}
  upper <- {}
  mC <- {}
  trapecio <- {}
  
  for (n in 1:nn){
    aux <- (to-from)*mean(sapply(runif(n, from, to), fun))
    mC <- c(mC, aux)
    S <- sd(mC)
    error <- qnorm(1-alfa)*S/sqrt(length(mC))
    lo <- aux-error
    up <- aux+error
    lower <- c(lower, lo)
    upper <- c(upper, up)
    
    aux <- trapezoid(n, from, to, fun)
    trapecio <- c(trapecio, aux)
  }
  aux <- data.frame(lowerLimit=lower, monteCarlo=mC, upperLimit=upper, trapecio=trapecio)
  return(aux)
}

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

shinyServer(function(input, output) {
  output$plot <- renderPlot({
    fun1 <- eval(parse(text = input$inpFunc))
    
    nn <- input$n
    from <- input$lmts[1]
    to <- input$lmts[2]
    to2 <- to - from
    
    u1 <- runif(nn, from, to)
    aux <- fun1(u1)
    aux[is.nan(aux)] <- 0
    #to2*mean(aux)
    
    curve(fun1, from=input$lmts[1], to=input$lmts[2], main=paste("MonteCarlo: ", to2*mean(aux)))
    
    aux <- fun1(seq(from, to))
    aux[is.nan(aux)] <- 0
    mn <- min(aux)
    mx <- max(aux)
    points(u1, runif(input$n, mn, mx))
  })
  
  output$intervals <- renderPlot({
    upper <- {}
    lower <- {}
    mC <- {}
    nn <- input$n
    
    fun1 <- eval(parse(text = input$inpFunc))
    
    for (i in seq(1,nn)){
      u1 <- runif(i, input$lmts[1], input$lmts[2])
      to2 <- input$lmts[2] - input$lmts[1]
      aux <- to2*mean(fun1(u1))
      mC[i]  <- aux
      
      s = sd(mC)
      error <- qnorm(1-input$alfa)*s/(sqrt(i))
      left <- aux-error
      right <- aux+error
      
      lower[i] <- left
      upper[i] <- right
    }
    
    plot(upper[2:nn], type="l", log="x",col="gray", main=paste("Simulación MonteCarlo: ", aux))
    lines(mC[2:nn], col="black")
    lines(lower[2:nn],  col="gray")
    abline(h=aux, col="red")
    grid()
  })
  
  output$comparation <- renderDataTable({
    fun1 <- eval(parse(text = input$inpFunc))
    aux <- mcIntervals(input$n, input$lmts[1], input$lmts[2], fun=fun1, alfa=input$alfa)
    aux
  })
})
