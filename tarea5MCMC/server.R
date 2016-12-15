rm(list=ls())
if(!require(shiny)) install.packages("shiny")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(DT)) install.packages("DT")
library(ggplot2)
library(DT)
library(shiny)
data <- read.csv(file="./cheese.csv", header=T)

Taste <- data$taste
data <- data[, !names(data) %in% c("id","taste")]

Rcpp::sourceCpp("funciones.cpp")

set.seed(28112016)

shinyServer(function(input, output) {
  n <- dim(data)[1]
  resultado <- {}
  ############################################################
  ####################--datos de entrada--##############################
  dataInput <- reactive({
    if(is.null(input$cVariables))
      return()
    aux <- cbind(Taste, data[, input$cVariables])
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
      # aux1 <- dataInput()[,1]
      # aux2 <- dataInput()[,2]
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
      chain <- runMCMC(x=data[, input$cVariables[1]], y=Taste, startValue=theta0, iterations=input$sLongitud)
      #chain <- data.frame(a=chain[,1], b=chain[,2], sd = chain[,3])
      #chain <- aux1
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
 df <- eventReactive(input$button, {
   if(is.null(input$cVariables))
     return()
   else{
     theta0 <- c(1,1,1)
     temp <- dataInput()
     chain <- runMCMC(x=temp[,2], y=Taste, startValue=theta0, iterations=input$sLongitud)
     chain <- data.frame(a=chain[,1], b=chain[,2], sd=chain[,3])
     for (i in 1:input$nCadenas-1){
       aux <- theta0 + round(10*runif(1))
       aux2 <- runMCMC(x=temp[,2], y=Taste, startValue=aux, iterations=input$sLongitud)
       aux2 <- data.frame(a=aux2[,1], b=aux2[,2], sd=aux2[,3])
       chain <- cbind(chain, aux2)
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
