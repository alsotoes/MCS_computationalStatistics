rm(list=ls())
if(!require(shiny)) install.packages("shiny")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(DT)) install.packages("DT")
library(ggplot2)
library(DT)
library(shiny)
#setwd("/Users/LUSI_ITAM/Documents/farid/estadisticaComputacional_Clase/tareas/tarea4_setupReaingTable")
#setwd("/home/farid/Documents/estadisticaComputacional/compstat2016/tarea5_MCMC")
data <- read.csv(file="cheese.csv", header=T)

Taste <- data$taste
data <- data[, !names(data) %in% c("id","taste")]

Rcpp::sourceCpp("funciones.cpp")

set.seed(28112016)
# n = 100
# A <- 1
# B <- 5
# C <- 3
# x <- seq(-10, 10, length.out = n)
# y <- A*x + B + rnorm(n, mean=0, sd=C)
##################################################################################
###########------declaring some functions--------##################################
# loglikelihood <- function(x, y, theta){        #Likelihood has normal distribution over theta
#   a   <- theta[1]
#   b   <- theta[2]
#   std <- theta[3]
#   
#   yy <- a*x+b
#   singleLikelihood <- dnorm(y, mean=yy, sd=std, log=T)
#   sumll <- sum(singleLikelihood)
#   return(sumll)
# }
# 
# logprior <- function(theta){         #priori add normal and unif probabilities densities
#   a   <- theta[1]
#   b   <- theta[2]
#   std <- theta[3]
#   
#   aa <- dunif(a, min=0, max=50, log=T)
#   bb <- dnorm(b, sd=5, log=T)     #jump size
#   std0 <- dunif(std, min=0, max=50, log=T)
#   return(aa+bb+std0)
# }
# 
# logposteriori <- function(x, y, theta){
#   return(loglikelihood(x, y, theta) + logprior(theta))
# }
# 
# proposal <- function(theta){
#   a   <- theta[1]
#   b   <- theta[2]
#   std <- theta[3]
#   #sd is jump size
#   return(rnorm(3, mean=c(a,b,std), sd=c(0.1, 0.5, 0.3)))
# }
# 
# runMCMC <- function(x, y, startValue, iterations){
#   chain <- array(dim = c(iterations+1,3))
#   chain[1,] <- startValue
#   for (i in 1:iterations){
#     prop <- proposal(chain[i,])
#     probab <- exp(logposteriori(x,y,prop)- logposteriori(x,y,chain[i,]))
#     if(runif(1) < probab){
#       chain[i+1, ] = prop
#     } else {
#       chain[i+1, ] = chain[i, ]
#       #i <- i-1
#     }
#   }
#   return(data.frame(a=chain[,1], b= chain[,2], sd = chain[,3]))
# }
############################################################################
############################################################################
ui <- fluidPage(
  titlePanel("Tarea 5: MCMC"),
  h3("Angel Farid Fajardo Oroz"),
  h4("MCC"),
  
  sidebarLayout(
    sidebarPanel(
     checkboxGroupInput("cVariables", h3("Variables"),
                              choices = names(data)),
     numericInput("nCadenas", "cadenas a simular", value=1, min=1, max=10, step=1),
     sliderInput("sLongitud", "longitud de cadenas", min=10000, max=1000000, value=1000),
     sliderInput("sBurnin", "Burnin", min=10, max=10000, value=5000),
     actionButton("button", "Calcula MCMC"),  #Calcula MCMC
     
     h4("Parámetros aPriori"),
     sliderInput("s_a", "a -> Unif ", min=1, max=10, value=c(5,8)),
     sliderInput("s_b", "b <- Norm", min=1, max=10, value=5),
     sliderInput("s_sigma", "sigma -> Unif", min=1, max=10, value=c(5, 6))
  ),
  
  mainPanel(
    tabsetPanel(type="tabs",
                tabPanel("datos", 
                         fluidRow(
                           column(8, plotOutput("plot_data")),
                           column(12, DT::dataTableOutput("table"))
                         )
                         ),
                tabPanel("distribuciones aPriori",
                         fluidRow(
                           column(4, plotOutput("plot_hist_A")),
                           column(4, plotOutput("plot_hist_B")),
                           column(4, plotOutput("plot_hist_Sd")),
                           column(4, plotOutput("plot_hist_Total"))
                          )
                         ),
                tabPanel("Parámetros de la regresión",
                         fluidRow(
                           column(4, plotOutput("hist_posteriori_A")),
                           column(4, plotOutput("hist_posteriori_B")),
                           column(4, plotOutput("hist_posteriori_Sd")),
                           column(4, plotOutput("plot_posteriori_A")),
                           column(4, plotOutput("plot_posteriori_B")),
                           column(4, plotOutput("plot_posteriori_Sd"))
                          )
                         ),
                tabPanel("Multiples cadenas",
                         fluidRow(
                           column(4, verbatimTextOutput("summary")),
                           column(4, plotOutput("regresionCalc")),
                           column(12, DT::dataTableOutput("cadenasMCMC"))
                         )),
                tabPanel("Convergencia de MCMC's", 
                         plotOutput("pConvergencia_A"),
                         plotOutput("pConvergencia_B"),
                         plotOutput("pConvergencia_Sd"))
                )
   )
  )
)

server <- function(input, output) {
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

}

shinyApp(ui = ui, server = server)

#shiny::runGitHub("compstat2016", "farid7", subdir = "tarea5_MCMC")