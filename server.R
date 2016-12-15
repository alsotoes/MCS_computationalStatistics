rm(list=ls())
if(!require(shiny)) install.packages("shiny")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(DT)) install.packages("DT")
if(!require(DT)) install.packages("shinydashboard")
if(!require(DT)) install.packages("Rcpp")
library(shiny)
library(DT)
library(shinydashboard)
library(ggplot2)
library(Rcpp)

set.seed(24082016)

########################################################
##########---Leyendo datos---##########################
data4 <- read.csv(file="cheese.csv", header=T)
Taste <- data4$taste
data5 <- data4[, !names(data4) %in% c("id","taste")]

#######################################################
##########---Funciones Rcpp---########################
Rcpp::sourceCpp("funciones.cpp")

########################################################
##########---Funcion auxiliar: GCL---##########################
LCG <- function(nsim, M = 2^32, a = 22695477, c = 1, seed = 110104){
  X = c(seed, numeric(nsim-1)) # Aparta memoria
  for(i in 1:(nsim-1)) X[i+1] <- ((a*X[i] + c)%% M) # Aplica GenradorCongruenciaLineal
  return(X/M) # Aplica transformacion
}

########################################################
##########---Funcion auxiliar: MonteCarlo Intervalos---##########################
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

########################################################
##########---Funcion auxiliar: Integral Trapecio---##########################
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
  ##########################################################
  #########---Generacion Numeros Aleatorios----###################
  data1 <- reactive({
    lambda = input$lambda
    switch(input$radioBtn,
           #generador de congruencia lineal, distribuci??n uniforme
           UNIF = {
             LCG(input$num)
             # sapply(aux[2:input$num], function(x, a=22695477, c=1, m=2**32){
             #   return(((a*x + c) %% m) / m)
             #   })
           },
           
           #Box-M??ller
           NORM = { #rnorm(input$num)
             u1 <- runif(input$num)   #R2 <- -2*log(u1)
             u2 <- runif(input$num)   #theta <- 2*pi*u2
             sqrt(-2*log(u1))*cos(2*pi*u2)
           },
           #funci??n inversa
           EXP  = {
             sapply(seq(1, input$num), function(x, lambda=input$lda){
               u <- runif(length(x))
               return(-log(1-u)/lambda)
             })
           },
           
           GEOM = {
             sapply(seq(1, input$num), function(x, prob=0.5){
               u <- runif(length(x))
               return(log(u)/log(1-prob))
             })
           })
  })
  
  kolmogorovTest <- reactive({
    switch(input$radioBtn,
           UNIF = ks.test(data1(), "punif"),
           NORM = ks.test(data1(), "pnorm"),
           EXP  = ks.test(data1(), "pexp"),
           GEOM = ks.test(data1(), rgeom(input$num, prob=0.5)))
  })
  
  chiTest <- reactive({
    switch(input$radioBtn,
           UNIF = {breaks <- seq(0,1, length.out = input$num/10)
           o <- table(cut(data1(), breaks=breaks))
           p <- diff(punif(breaks))
           chisq.test(o, p=p, rescale.p=T)},
           NORM = {breaks <- c(seq(-5,5, length.out = input$num/10))
           o <- table(cut(data1(), breaks = breaks))
           p <- diff(pnorm(breaks))
           chisq.test(o, p=p, rescale.p = T)},
           EXP  = {breaks <- c(seq(0,10, length.out = input$num/10))
           o <- table(cut(data1(), breaks = breaks))
           p <- diff(pexp(breaks))
           chisq.test(o, p=p, rescale.p = T)},
           GEOM = {breaks <- c(seq(0,10, by=1))
           o <- table(cut(data1(), breaks = breaks))
           p <- diff(pgeom(breaks, prob=0.5))
           chisq.test(o, p=p, rescale.p = T)}
    )
  })
  
  output$ksTest <- renderPrint({
    kolmogorovTest()
  })
  
  output$chiTest <- renderPrint({
    chiTest()
  })
  
  output$stats <- renderPrint({
    summary(data1())
  })
  
  output$hist <- renderPlot({
    h <- hist(data1(), breaks = input$bins, plot=F)
    d <- density(data1())
    hist(data1(), breaks = input$bins,
         main= isolate(input$title))
    lines(x=d$x, y=d$y*length(data1())*diff(h$breaks)[1], ldw=2)
  })
  
  output$qqPlot <- renderPlot({
    par(mfrow=c(1,2))
    switch(input$radioBtn,
           UNIF = {q1 = qunif(seq(0,1,0.01)); 
           q2 = quantile(data1(), seq(0,1,0.01));
           plot(q1, q2, main="Q-Q Plot", ylab = "CLG", xlab="qunif")},
           NORM = {q1 = qnorm(seq(0,1,0.01)); 
           q2 = quantile(data1(), seq(0,1,0.01));
           plot(q1, q2, main="Q-Q Plot", ylab="rnorm", xlab="qnorm")},
           EXP  = {q1 = qexp(seq(0,1,0.01));  
           q2 = quantile(data1(), seq(0,1,0.01));
           plot(q1, q2, main="Q-Q Plot", ylab = "expInv", xlab="qexp")},
           GEOM = {q1 = qgeom(seq(0,1,0.01), prob=0.5);  
           q2 = quantile(data1(), seq(0,1,0.01));
           plot(q1, q2, main="Q-Q Plot", ylab = "geomInv", xlab="qgeom")}
    )
    plot(data1()[1:length(data1())-1], data1()[2:length(data1())], main = "Secuencia en n??meros")
    #qplot(data1()[-length(data1())], data1()[-1], main = "Secuencia en n??meros")
  })
  output$distPlot <- renderPlot({
    
    if (input$tab == "random") {
    }    

    
  })
  output$downloadData <- downloadHandler(
    filename = function() { paste(input$radioBtn, '.csv', sep='') },
    content = function(file) {
      write.csv(data1(), file)
    }
  )
  
  ##########################################################
  #########---Integracion MonteCarlo----###################
  output$plot <- renderPlot({
    fun1 <- eval(parse(text = input$inpFunc))
    
    nn <- input$n
    from <- input$lmts[1]
    to <- input$lmts[2]
    to2 <- to - from
    
    u1 <- runif(nn, from, to)
    aux <- fun1(u1)
    aux[is.nan(aux)] <- 0

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
  ###########################################################
  ##########---Set Up inicial----###########################
  t4_n <- dim(data4)[1]
  
  t4_dataInput <- reactive({
    if(is.null(input$t4_cVariables))
      return()
    aux <- data4[, input$t4_cVariables]
    aux
  })
  
  t4_priori_a <- reactive({
    runif(t4_n, min=input$t4_s_a[1], max = input$t4_s_a[2])
  })
  t4_priori_b <- reactive({
    rnorm(t4_n, mean=0, sd = input$t4_s_b)
  })
  t4_priori_sd <- reactive({
    runif(t4_n, min=input$t4_s_sigma[1], max = input$t4_s_sigma[2])
  })
  
  t4_nmes <- renderText({
    input$t4_cVariables
  })
  
  output$t4_table <- DT::renderDataTable(DT::datatable({
    if(is.null(input$t4_cVariables))
      return()
    else 
      return(t4_dataInput())
    
  }))
  
  output$t4_plot_data <- renderPlot({
    if(is.null(input$t4_cVariables))
      return()
    else{
      return(plot(t4_dataInput(), main="Grafica de dispersion para quesos"))
    }
  })
  
  output$t4_plot_hist_A <- renderPlot({
    hist(t4_priori_a())
  })
  
  output$t4_plot_hist_B <- renderPlot({
    hist(t4_priori_b())
  })
  
  output$t4_plot_hist_Sd <- renderPlot({
    hist(t4_priori_sd())
  })
  
  output$t4_plot_hist_Total <- renderPlot({
    hist(t4_priori_a() * t4_priori_b() * t4_priori_sd(), main="distribucin aPriori")
  })
  
  
  
  ##########################################################
  ############---Regresion Bayesiana: MCMC---##############
  n <- dim(data5)[1]
  resultado <- {}
  ############################################################
  ####################--datos de entrada--##############################
  dataInput <- reactive({
    if(is.null(input$cVariables))
      return()
    aux <- cbind(Taste, data5[, input$cVariables])
    aux
  })

  ############################################################
  ####################--grafica de entrada--#########################
  output$table <- DT::renderDataTable(DT::datatable({
    if(is.null(input$cVariables))
      return()
    else
      return(dataInput())

  }))

  output$plot_data <- renderPlot({
    if(is.null(input$cVariables))
      return()
    else{
      return(plot(dataInput(), main="Grafica de dispersion para quesos"))
    }
  })

  ############################################################
  ####################--Variables aPriori--##############################
  output$plot_hist_A <- renderPlot({
    priori_a <- runif(n, min=input$s_a[1], max = input$s_a[2])
    hist(priori_a)
  })

  output$plot_hist_B <- renderPlot({
    priori_b <- rnorm(n, mean=0, sd = input$s_b)
    hist(priori_b)
  })

  output$plot_hist_Sd <- renderPlot({
    priori_sd <- runif(n, min=input$s_sigma[1], max = input$s_sigma[2])
    hist(priori_sd)
  })

  ############################################################
  ####################--MCMC--##############################
  chain <- reactive({
    if(is.null(input$cVariables))
      return()
    else{
      theta0 <- c(1,1,1)
      temp <- dataInput()
      chain <- runMCMC(x=data5[, input$cVariables[1]], y=Taste, startValue=theta0, iterations=input$sLongitud)
      return(data.frame(a=chain[,1], b=chain[,2], sd=chain[,3]))
    }
  })

  output$table <- DT::renderDataTable(DT::datatable({
    if(is.null(input$cVariables))
      return()
    else {
      return(dataInput())
    }

  }))

  output$Graph1 <- renderPlot({
    if(is.null(input$cVariables))
      return()
    else{
      return(plot(dataInput()))
    }
  })

  output$gPriori <- renderPlot({
    #aux <- runMCMC(x, y, theta0, 10000)
    hist(chain()[,1], title=paste("MCMC"))
  })

  ####################################################
  ############---Calcula MCMC con botòn---################
  df <- eventReactive(input$button, {              #dataframe with multiple chains
    if(is.null(input$cVariables))
      return()
    else{
      theta0 <- c(1,1,1)
      temp <- dataInput()
      chain <- runMCMC(x=temp[,2], y=Taste, startValue=theta0, iterations=input$sLongitud)
      chain <- data.frame(a=chain[,1], b=chain[,2], sd=chain[,3])
      if(input$nCadenas > 1){
       for (i in 2:input$nCadenas){
        aux <- theta0 + round(10*runif(1))
        aux2 <- runMCMC(x=temp[,2], y=Taste, startValue=aux, iterations=input$sLongitud)
        aux2 <- data.frame(a=aux2[,1], b=aux2[,2], sd=aux2[,3])
        chain <- cbind(chain, aux2)
       }
      }
      return(chain)
    }

  })

  output$cadenasMCMC <- DT::renderDataTable(DT::datatable({
    if(is.null(df()))
      return()
    else
      return(df())
  }))


  ##############################################################################
  output$hist_posteriori_A <- renderPlot({
    if(is.null(df()))
      return()
    else{
      return({AA = mean(df()[-(1:input$sBurnin),1])
      hist(df()[,1], main=paste("Posterior A: ", AA))
      abline(v=AA, col="red")
      })
    }
  })
  output$hist_posteriori_B <- renderPlot({
    if(is.null(df()))
      return()
    else{
      return({BB = mean(df()[-(1:input$sBurnin),2])
      hist(df()[,2], main=paste("Posterior B: ", BB))
      abline(v=BB, col="red")
      })
    }
  })
  output$hist_posteriori_Sd <- renderPlot({
    if(is.null(df()))
      return()
    else{
      return({Ssd = mean(df()[-(1:input$sBurnin),3])
      hist(df()[,3], main=paste("Posterior A: ", Ssd))
      abline(v=Ssd, col="red")
      })
    }
  })
  output$plot_posteriori_A <- renderPlot({
    if(is.null(df()))
      return()
    else{
      return({AA = mean(df()[-(1:input$sBurnin),1])
      plot(df()[,1], type="l", main=paste("Posterior A: ", AA))
      abline(h=AA, col="red")
      })
    }
  })
  output$plot_posteriori_B <- renderPlot({
    if(is.null(df()))
      return()
    else{
      return({BB = mean(df()[-(1:input$sBurnin),2])
      plot(df()[,2], type="l", main=paste("Posterior B: ", BB))
      abline(h=BB, col="red")
      })
    }
  })
  output$plot_posteriori_Sd <- renderPlot({
    if(is.null(df()))
      return()
    else{
      return({Ssd = mean(df()[-(1:input$sBurnin),3])
      plot(df()[,3], type="l", main=paste("Posterior Sd: ", Ssd))
      abline(h=Ssd, col="red")
      })

    }
  })
  
  output$plot_hist_Afinal <- renderPlot({
    if(is.null(df()))
      return()
    else{
      return({AA = mean(df()[-(1:input$sBurnin),1])
      h <- hist(df()[,1], plot=F, breaks=10)
      d <- density(df()[,1])
      hist(df()[,1], main=paste("Posterior A: ", AA),  breaks=10)
      abline(v=AA, col="red")
      lines(x=d$x, y=d$y*length(df()[,1])*diff(h$breaks)[1], ldw=2)
      })
    }
  })
  output$plot_hist_Bfinal <- renderPlot({
    if(is.null(df()))
      return()
    else{
      return({
      BB = mean(df()[-(1:input$sBurnin),2])

      h <- hist(df()[,2], plot=F, breaks=10)
      d <- density(df()[,2])
      hist(df()[,2], main=paste("Posterior B: ", BB),  breaks=10)
      abline(v=BB, col="red")
      lines(x=d$x, y=d$y*length(df()[,2])*diff(h$breaks)[1], ldw=2)
      })
    }
  })
  output$plot_hist_Sdfinal <- renderPlot({
    if(is.null(df()))
      return()
    else{
      return({Ssd = mean(df()[-(1:input$sBurnin),3])
      
      # hist(df()[,3], main=paste("Posterior A: ", Ssd))
      # abline(v=Ssd, col="red")
      
      h <- hist(df()[,3], plot=F, breaks=10)
      d <- density(df()[,3])
      hist(df()[,3], main=paste("Posterior B: ", Ssd),  breaks=10)
      abline(v=Ssd, col="red")
      lines(x=d$x, y=d$y*length(df()[,3])*diff(h$breaks)[1], ldw=2)
      })
    }
  })

  ##############################################################################
  output$regresionCalc <- renderPlot({
    if(is.null(input$cVariables))
      return()
    else
      return({
        plot(dataInput()[,2], dataInput()[,1])
        lines(dataInput()[,2], dataInput()[,2]*mean(df()[,1]) + mean(df()[,2]), col="blue")
      })
  })

  ##############################################################################
  output$pConvergencia_A <- renderPlot({
    if(is.null(df()))
      return()
    else{
      par(mfrow=(c(1,1)))
      return({plot(df()[,1], type = "l", main="Parametro A")
        lines(df()[,4], col="red")
        lines(df()[,7], col="blue")
        lines(df()[,10], col="green")
        lines(df()[,13], col="black")
      })
    }
  })
  output$pConvergencia_B <- renderPlot({
    if(is.null(df()))
      return()
    else{
      par(mfrow=(c(1,1)))
      return({plot(df()[,2], type = "l", main="Parametro B")
        lines(df()[,5], col="red")
        lines(df()[,8], col="blue")
        lines(df()[,11], col="green")
        lines(df()[,14], col="black")
      })
    }
  })
  output$pConvergencia_Sd <- renderPlot({
    if(is.null(df()))
      return()
    else{
      par(mfrow=(c(1,1)))
      return({plot(df()[,3], type = "l", main="Parametro Sd")
        lines(df()[,6], col="red")
        lines(df()[,9], col="blue")
        lines(df()[,12], col="green")
        lines(df()[,15], col="black")
      })
    }
  })
})

#shiny::runGitHub("compstat2016", "farid7")